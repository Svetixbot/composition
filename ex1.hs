-- **** Challenge #1. The Magic Hat **** --
-- Magic hat takes 
-- an array of languages
-- an array of developers
-- an array of projects
-- 
-- It produces an array of Possibilities. 
-- Possibilities are possibile developer's allocations to passed projects
-- with respect to developer's programming language preference

-- for-each language
--    you can find a list of developers who like this language
--        and for-each found developer
--          there will be an array of projects which use this language
--            and for-each found project
--              you can create a possibility

import Control.Monad
import Data.Functor

-- Those are defined types for the exercise:
-- They are like classes with contractors and getters.
-- To create new language use:
--  let newLanguage = Language "Java"
-- To access properties of language:
--  name newLanguage
data Language = Language {
  name :: String 
} deriving (Show, Eq) 

data Developer = Developer {
  prefered :: Language
} deriving (Show, Eq)

data Project = Project { 
  title :: String,
  language :: Language
} deriving (Show, Eq)

data Possibility = Possibility {
  developer :: Developer,
  project :: Project
} deriving (Show, Eq)

-- ***** exercise #1 ***** --
-- Implement isPrefered boolean predicate
-- It returns true if passed language is prefered developer's language
isPrefered :: Language -> Developer -> Bool
isPrefered lang dev = lang == prefered dev

-- ***** exercise #2 ***** --
-- Implement isProjectLanguage boolean predicate
-- It returns true if project is using passed language
isProjectLanguage :: Language -> Project -> Bool
isProjectLanguage lang prj = lang == language prj


-- ***** exercise #3 ***** --
-- Implement build possibilities function
--
-- Hint #1
-- Array if a Functor, therefore you can use this function:
-- (<$>) :: (a -> b) -> f a -> f b 
--
-- where (<$>) is the name of function
-- (a -> b) is a function from a to b, in our case 
--          a is a project and b is a Possibility
-- f a - is a functor of type a, in our case it equals [Project]
-- f b - is a return type of this funciton, in our case it equals [Possibilities]

-- Hint #2
-- Partial application!
-- constractors of types are just functions, therefore they can be partialy applied.
-- This is our Possibility function, which creates a Possibility object:
-- Possibility :: Developer -> Project -> Possibility
-- (Possibility dev) will return a function of type (Project -> Possibility)

buildPossibilities:: [Project] -> Developer -> [Possibility]
buildPossibilities projects dev = (Possibility dev) <$> projects


-- ***** exercise #4 ***** --
-- Implement magic hat using monadic bind:
-- (>>=) :: m a -> (a -> m b) -> m b 
-- where 
-- m a - Monad of type a, in our case it's a an array.
-- (a -> m b) - it's a function which is applicable to the element from monad, 
-- in our case it's a function which takes a single element on an array 
-- and returns an array of different type, like:
-- Language -> [Developer]
magicHatBind :: [Language] -> [Developer] -> [Project] -> [Possibility]
magicHatBind langs devs projects = 
  langs >>= (\lang -> 
                (filter (isPrefered lang) devs) >>= 
                  (buildPossibilities $ filter (isProjectLanguage lang) projects))


-- ***** exercise #5 ***** --
-- Implement the same magic hat 
-- but with do-notation
-- Do-notation is a syntatic sugar for bind operation
magicHat :: [Language] -> [Developer] -> [Project] -> [Possibility]
magicHat langs devs projects = do 
  lang <- langs
  dev <- filter (isPrefered lang) devs
  project <- filter (isProjectLanguage lang) projects
  return (Possibility dev project)