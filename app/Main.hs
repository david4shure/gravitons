import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import System.Random
import Debug.Trace
import Data.Fixed

-- "globals"
width = 800
height = 800
fwidth = fromIntegral width :: Float
fheight = fromIntegral height :: Float
forcefactor = 15
ghostparticlemass = 250000
baseparticlemass  = 1000
eta = 10
dragperframe = 0.98
numParticles = 1500 :: Int
g            = (6.674*10)^11

colors = [(dark (dark green)), (dark (dark red))]

-- datas

data Particle = Particle
            {
              xPos     :: Float,
              yPos     :: Float,
              xVel     :: Float,
              yVel     :: Float,
              mass     :: Float,
              radius   :: Float,
              sign     :: Float,
              colorIdx :: Int
            } deriving (Show, Eq)

data WorldState = WorldState
                {
                  particles :: [Particle],
                  ghosts :: [Particle]
                } deriving (Show, Eq)

-- helper functions
-- drawingFunc :: WorldState -> Picture
-- drawingFunc _ = translate 30 40 (Circle 20)

particle2pic :: Particle -> Picture
particle2pic p = translate particleX particleY $color (colors !! (colorIdx p)) $circleSolid (radius p)
  where
    particleX = (xPos p)
    particleY = (yPos p)


particle2line :: Particle -> Picture
particle2line p = color red $line (getVectorHeading p)

getVectorHeading :: Particle -> [(Float,Float)]
getVectorHeading p = [vStart,vEnd]
  where
    particleX = (xPos p)
    particleY = (yPos p)
    vMag      = sqrt ((xVel p) ^ 2 + (yVel p) ^ 2)
    vUnit     = ((xVel p)/vMag,(yVel p)/vMag)
    vScaled   = ((fst vUnit) * 8, (snd vUnit) * 8)
    vEnd      = (particleX+(fst vScaled),particleY+(snd vScaled))
    vStart    = (particleX,particleY)


drawingFunc :: WorldState -> Picture
drawingFunc ws = pictures allparticles
  where
    allparticles = (concat . map (\p -> [(particle2pic p), (particle2line p)])) $particles ws

inputHandler :: Event -> WorldState -> WorldState
inputHandler (EventKey (SpecialKey KeyUp) Down _ _) ws = ws
inputHandler (EventKey (SpecialKey KeyDown) Down _ _) ws = ws
inputHandler (EventKey (SpecialKey KeyRight) Down _ _) ws = ws
inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) ws = ws
inputHandler (EventKey (MouseButton RightButton) Down _ _) ws = nws where
  newparticles = particles ws
  newghosts    = ghosts ws
  n            = length newghosts
  nws          = WorldState { particles = newparticles, ghosts = take (n-1) newghosts }

inputHandler (EventKey (MouseButton LeftButton) Down _ (x', y')) ws = newws where
  newparticles = (particles ws)
  newghosts    = (ghosts ws) ++ [
                               Particle {
                                 xVel = 0,
                                 yVel = 0,
                                 xPos = x',
                                 yPos = y',
                                 mass = ghostparticlemass,
                                 radius = 5,
                                 sign   = -1.0,
                                 colorIdx = (round (x'+y') `mod` length colors)
                               }
                             ]
  newws = WorldState { particles = newparticles, ghosts = newghosts }

inputHandler _ w = w

updateFunc :: Float -> WorldState -> WorldState
-- updateFunc t ws | trace (show ws ++ "\n\n\n") False = undefined
updateFunc t ws = newws where
  ps = (particles ws)
  gs = (ghosts ws)
  newws = WorldState {
    particles = map (\p -> updateParticle t p ps gs) ps,
    ghosts = gs
  }

updateParticle :: Float -> Particle -> [Particle] -> [Particle]-> Particle
updateParticle t p ps gs = newparticle where
                          ghostLeft = Particle {
                            xVel = 0,
                            yVel = 0,
                            xPos = (fromIntegral (-width)::Float) / 2.0,
                            yPos = (yPos p),
                            mass = ghostparticlemass,
                            radius = 3,
                            sign   = -1.0,
                            colorIdx = 0
                          }

                          ghostRight = Particle {
                            xVel = 0,
                            yVel = 0,
                            xPos = (fromIntegral width::Float) / 2.0,
                            yPos = (yPos p),
                            mass = ghostparticlemass,
                            radius = 3,
                            sign   = -1.0,
                            colorIdx = 0
                          }

                          ghostBottom = Particle {
                            xVel = 0,
                            yVel = 0,
                            xPos = (xPos p),
                            yPos = (fromIntegral (-height)::Float) / 2.0,
                            mass = ghostparticlemass,
                            radius = 3,
                            sign   = -1.0,
                            colorIdx = 0
                          }

                          ghostTop = Particle {
                            xVel = 0,
                            yVel = 0,
                            xPos = (xPos p),
                            yPos = (fromIntegral height::Float) / 2.0,
                            mass = ghostparticlemass,
                            radius = 3,
                            sign   = -1.0,
                            colorIdx = 0
                          }

                          -- force = netForceVector t p ([] ++ ps) -- (3.0,2.7)
                          force = netForceVector t p ([ghostLeft,ghostRight,ghostTop,ghostBottom] ++ ps ++ gs) -- (3.0,2.7)
                          netforce = if (isNaN (fst force)) || (isNaN (snd force)) then (0.0,0.0) else scaleVector force (sign p)
                          netacc   = ((fst netforce) / (mass p), (snd netforce) / (mass p))
                          vfinal   = scaleVector netacc t
                      
                          newxvel    = (fst vfinal)
                          newyvel    = (snd vfinal)

                          finalxvel  = ((xVel p) + newxvel) * dragperframe
                          finalyvel  = ((yVel p) + newyvel) * dragperframe

                          origxpos   = (xPos p)
                          origypos   = (yPos p)

                          newxpos    = (origxpos + (finalxvel * t))
                          newypos    = (origypos + (finalyvel * t))

                          halfx      = (fwidth/2.0)
                          halfy      = (fheight/2.0)

                          -- 2d canvas should behave like taurus, wrap left to right, top to bottom
                          finalxpos  = if newxpos < (-halfx) then
                                         newxpos `mod'` halfx
                                       else if newxpos >= halfx then
                                         newxpos `mod'` (-halfx)
                                       else
                                         newxpos
                                           
                          finalypos  = if newypos < (-halfy) then
                                         newypos `mod'` halfy
                                       else if newypos >= halfy then
                                         newypos `mod'` (-halfy)
                                       else
                                         newypos

                          dotp = dot (finalxvel, finalyvel) (finalxpos,finalypos)

                          newparticle = Particle {
                            xVel   = finalxvel,
                            yVel   = finalyvel,
                            xPos   = finalxpos,
                            yPos   = finalypos,
                            mass   = (mass p),
                            radius = (radius p),
                            sign   = (sign p),
                            colorIdx = if dotp >= 0.0 then 0 else 1
                          }

dot :: (Float,Float) -> (Float,Float) -> Float
dot t1 t2 = (fst t1 * fst t2) + (snd t1 * snd t2)

getForceVector :: Float -> Particle -> Particle -> (Float,Float)
getForceVector t p1 p2 | p1 == p2  = (0.0,0.0)
                       | otherwise = force where
                           p_displace     = ((xPos p2) - (xPos p1), (yPos p2) - (yPos p1))
                           mag_displace   = sqrt ((fst p_displace) ^ 2 + (snd p_displace) ^ 2)
                           p_scaled       = (((fst p_displace) / mag_displace), ((snd p_displace) / mag_displace))
                           force_mag      = forcefactor * ((mass p1) * (mass p2)) / (mag_displace ^2)
                           force          = if distance p1 p2 <= 6 then scaleVector p_scaled 20000 else scaleVector p_scaled force_mag

distance :: Particle -> Particle -> Float
distance p1 p2 = sqrt(((xPos p1) - (xPos p2))^2 + ((yPos p1) - (yPos p2))^2)

netForceVector :: Float -> Particle -> [Particle] -> (Float,Float)
netForceVector t p = sumTuples . map (\p' -> getForceVector t p p') . filter (\p' -> distance p p' <= 100.0)

sumTupleArr :: [(Float,Float)] -> Float -> Float -> (Float,Float)
sumTupleArr [] lsum rsum         = (lsum,rsum)
sumTupleArr ((x,y):[]) lsum rsum = (x+lsum,y+rsum)
sumTupleArr ((x,y):ts) lsum rsum = sumTupleArr ts (lsum + x) (rsum + y)

sumTuples :: [(Float,Float)] -> (Float,Float)
sumTuples arr = (sum [x | (x,_) <- arr], sum [y | (_,y) <- arr])

scaleVector :: (Float,Float) -> Float -> (Float,Float)
scaleVector v s = (s * fst v, s * snd v)


particle :: (Float,Float) -> (Float,Float) -> Particle
particle (x,y) (vx,vy) = particle where 
  particle = Particle { xPos = x, yPos = y, xVel = vx, yVel = vy, mass = baseparticlemass, radius = 3, sign = (-1.0),colorIdx = if (dot (vx,vy) (0,0)) <= 0 then 0 else 1 }

genparticles :: Int -> [(Float,Float)] -> [(Float,Float)] -> [Particle]
genparticles n randPosns randVels = particles
  where
    particles = map (\i -> particle (randPosns !! i) (randVels !! i)) [0..n-1]

getInitialState :: Int -> [(Float,Float)] -> [(Float,Float)] -> WorldState
getInitialState numParticles randPosns randVels = WorldState { particles = (genparticles numParticles randPosns randVels), ghosts = [] }

getRandPairs :: Int -> Int -> Int -> Int -> Int -> IO [(Float,Float)]
getRandPairs _ _ _ _ 0 = return []
getRandPairs fst_left_rng fst_right_rng snd_left_rng snd_right_rng n = do
                  w <- randomRIO (fst_left_rng + eta, fst_right_rng - eta)
                  h <- randomRIO (snd_left_rng + eta, snd_right_rng - eta)
                  ns <- getRandPairs fst_left_rng fst_right_rng snd_left_rng snd_right_rng (n-1)
                  return ((fromIntegral w::Float,fromIntegral h::Float):ns)

windowDisplay :: Display
windowDisplay = InWindow "Gravitons" (width, height) (width `div` 2, height `div` 2)

-- main function
main :: IO ()
main = do
  print "Generating point pairs..."
  
  posnpairs <- getRandPairs ((-width)`div`2) (width`div`2) ((-height)`div`2) (height`div`2) numParticles
  velpairs  <- getRandPairs (-100) 100 (-100) 100 numParticles

  print "Simulating..."

  (play windowDisplay white 20 (getInitialState numParticles posnpairs velpairs) drawingFunc inputHandler updateFunc)
