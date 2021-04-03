module React.Basic.Emotion
  ( Style
  , StyleProperty
  , class IsStyle
  , style
  , class IsStyleProperty
  , prop
  , element
  , elementKeyed
  , css
  , global
  , important
  , keyframes
  , nested
  , merge
  , str
  , int
  , num
  , fallbacks
  , url
  , color
  , none
  , auto
  , inherit
  , unset
  , initial
  , revert
  , borderBox
  , contentBox
  , hidden
  , visible
  , scroll
  , wrap
  , nowrap
  , row
  , column
  , default
  , manipulation
  , pointer
  , solid
  , ellipsis
  , underline
  , fixed
  , absolute
  , relative
  , sticky
  , block
  , inlineBlock
  , flex
  , inlineFlex
  , grid
  , inlineGrid
  , flexStart
  , flexEnd
  , center
  , stretch
  , baseline
  , spaceAround
  , spaceBetween
  , spaceEvenly
  , minContent
  , maxContent
  , preWrap
  , var
  , px, px', cm, mm, inches, pt, pc
  , em, ex, ch, rem, vw, vh, vmin, vmax, percent
  , px2, px2', cm2, mm2, inches2, pt2, pc2
  , em2, ex2, ch2, rem2, vw2, vh2, vmin2, vmax2, percent2
  , px4, px4', cm4, mm4, inches4, pt4, pc4
  , em4, ex4, ch4, rem4, vw4, vh4, vmin4, vmax4, percent4
  ) where

import Prelude

import Color (Color, cssStringHSLA)
import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Int as Int
import Data.Number.Format (toString) as Number
import Data.String as String
import Foreign as F
import Foreign.Object (Object, fromHomogeneous)
import React.Basic (JSX, ReactComponent)
import Type.Row.Homogeneous (class Homogeneous)
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML.History (URL(..))

data Style

instance semigroupStyle :: Semigroup Style where
  append x y =
    let
      xF = F.unsafeToForeign x

      yF = F.unsafeToForeign y
    in
      case runExcept $ F.readArray xF, runExcept $ F.readArray yF of
        Right xArr, Right yArr -> merge (unsafeCoerce xArr <> unsafeCoerce yArr)
        Left _, Right yArr -> merge (Array.cons x (unsafeCoerce yArr))
        Right xArr, Left _ -> merge (Array.snoc (unsafeCoerce xArr) y)
        Left _, Left _ -> merge [ x, y ]

instance monoidStyle :: Monoid Style where
  mempty = emptyStyle

foreign import emptyStyle :: Style

class IsStyle a where
  style :: a -> Style

instance isStyleStyle :: IsStyle Style where
  style = identity

data StyleProperty

instance semigroupStyleProperty :: Semigroup StyleProperty where
  append x y =
    let
      xF = F.unsafeToForeign x

      yF = F.unsafeToForeign y
    in
      case runExcept $ F.readArray xF, runExcept $ F.readArray yF of
        Right xArr, Right yArr -> fallbacks (unsafeCoerce xArr <> unsafeCoerce yArr)
        Left _, Right yArr -> fallbacks (Array.cons x (unsafeCoerce yArr))
        Right xArr, Left _ -> fallbacks (Array.snoc (unsafeCoerce xArr) y)
        Left _, Left _ -> fallbacks [ x, y ]

instance monoidStyleProperty :: Monoid StyleProperty where
  mempty = emptyStyleProperty

foreign import emptyStyleProperty :: StyleProperty

class IsStyleProperty a where
  prop :: a -> StyleProperty

instance isStylePropertyStyleProperty :: IsStyleProperty StyleProperty where
  prop = identity

-- | Create a `JSX` node from a `ReactComponent`, by providing the props.
-- |
-- | This function is identical to `React.Basic.element` plus Emotion's
-- | `css` prop.
element ::
  forall props.
  ReactComponent { className :: String | props } ->
  { className :: String, css :: Style | props } ->
  JSX
element = runFn2 element_

foreign import element_ ::
  forall props.
  Fn2
    (ReactComponent { className :: String | props })
    { className :: String, css :: Style | props }
    JSX

-- | Create a `JSX` node from a `ReactComponent`, by providing the props.
-- |
-- | This function is identical to `React.Basic.element` plus Emotion's
-- | `css` prop.
elementKeyed ::
  forall props.
  ReactComponent { className :: String | props } ->
  { key :: String, className :: String, css :: Style | props } ->
  JSX
elementKeyed = runFn2 elementKeyed_

foreign import elementKeyed_ ::
  forall props.
  Fn2
    (ReactComponent { className :: String | props })
    { key :: String, className :: String, css :: Style | props }
    JSX

foreign import global :: ReactComponent { styles :: Style }

css :: forall r. Homogeneous r StyleProperty => { | r } -> Style
css = css_ <<< fromHomogeneous

foreign import css_ :: Object StyleProperty -> Style

foreign import important :: StyleProperty -> StyleProperty

keyframes :: forall r. Homogeneous r Style => { | r } -> StyleProperty
keyframes = keyframes_ <<< fromHomogeneous

foreign import keyframes_ :: Object Style -> StyleProperty

nested :: Style -> StyleProperty
nested = unsafeCoerce

merge :: Array Style -> Style
merge = unsafeCoerce

str :: String -> StyleProperty
str = unsafeCoerce

-- | Create a unitless `Int` property.
-- |
-- | Wherever units may be specified, use unit combinators like `px`
-- | or `em` instead.
int :: Int -> StyleProperty
int = unsafeCoerce

-- | Create a unitless `Number` property.
-- |
-- | Wherever units may be specified, use unit combinators like `px`
-- | or `em` instead.
num :: Number -> StyleProperty
num = unsafeCoerce

fallbacks :: Array StyleProperty -> StyleProperty
fallbacks = unsafeCoerce

url :: URL -> StyleProperty
url (URL url') = str ("url(" <> url' <> ")")

color :: Color -> StyleProperty
color = str <<< cssStringHSLA

none :: StyleProperty
none = str "none"

auto :: StyleProperty
auto = str "auto"

inherit :: StyleProperty
inherit = str "inherit"

unset :: StyleProperty
unset = str "unset"

initial :: StyleProperty
initial = str "initial"

revert :: StyleProperty
revert = str "revert"

borderBox :: StyleProperty
borderBox = str "border-box"

contentBox :: StyleProperty
contentBox = str "content-box"

hidden :: StyleProperty
hidden = str "hidden"

visible :: StyleProperty
visible = str "visible"

scroll :: StyleProperty
scroll = str "scroll"

wrap :: StyleProperty
wrap = str "wrap"

nowrap :: StyleProperty
nowrap = str "nowrap"

row :: StyleProperty
row = str "row"

column :: StyleProperty
column = str "column"

default :: StyleProperty
default = str "default"

manipulation :: StyleProperty
manipulation = str "manipulation"

pointer :: StyleProperty
pointer = str "pointer"

solid :: StyleProperty
solid = str "solid"

ellipsis :: StyleProperty
ellipsis = str "ellipsis"

underline :: StyleProperty
underline = str "underline"

fixed :: StyleProperty
fixed = str "fixed"

absolute :: StyleProperty
absolute = str "absolute"

relative :: StyleProperty
relative = str "relative"

sticky :: StyleProperty
sticky = str "sticky"

block :: StyleProperty
block = str "block"

inlineBlock :: StyleProperty
inlineBlock = str "inline-block"

flex :: StyleProperty
flex = str "flex"

inlineFlex :: StyleProperty
inlineFlex = str "inline-flex"

grid :: StyleProperty
grid = str "grid"

inlineGrid :: StyleProperty
inlineGrid = str "inline-grid"

flexStart :: StyleProperty
flexStart = str "flex-start"

flexEnd :: StyleProperty
flexEnd = str "flex-end"

center :: StyleProperty
center = str "center"

stretch :: StyleProperty
stretch = str "stretch"

baseline :: StyleProperty
baseline = str "baseline"

spaceAround :: StyleProperty
spaceAround = str "space-around"

spaceBetween :: StyleProperty
spaceBetween = str "space-between"

spaceEvenly :: StyleProperty
spaceEvenly = str "space-evenly"

minContent :: StyleProperty
minContent = str "min-content"

maxContent :: StyleProperty
maxContent = str "max-content"

preWrap :: StyleProperty
preWrap = str "pre-wrap"

-- | Use a variable name as a property.
-- |
-- | Define a property somewhere:
-- | ```
-- | css { "--color-primary": color blue }
-- | ```
-- |
-- | Use the var:
-- | ```
-- | css { color: var "--color-primary" }
-- | ```
var :: String -> StyleProperty
var n = str ("var(" <> n <> ")")

-- Absolute length units

-- | Pixels. This function does not take a `Number` because approaches to
-- | subpixel rendering vary among browser implementations.
px :: Int -> StyleProperty
px = toUnitWith (Int.toStringAs Int.decimal) "px"

px2 :: Int -> Int -> StyleProperty
px2 = toUnitWith2 (Int.toStringAs Int.decimal) "px"

px4 :: Int -> Int -> Int -> Int -> StyleProperty
px4 = toUnitWith4 (Int.toStringAs Int.decimal) "px"

-- | Pixels and subpixels.
-- |
-- | WARNING: Approaches to subpixel rendering vary among browser
-- | implementations. This means that non-integer pixel values may be displayed
-- | differently in different browsers.
px' :: Number -> StyleProperty
px' = toUnitWith Number.toString "px"

px2' :: Number -> Number -> StyleProperty
px2' = toUnitWith2 Number.toString "px"

px4' :: Number -> Number -> Number -> Number -> StyleProperty
px4' = toUnitWith4 Number.toString "px"

-- | Centimeters
cm :: Number -> StyleProperty
cm = toUnitWith Number.toString "cm"

cm2 :: Number -> Number -> StyleProperty
cm2 = toUnitWith2 Number.toString "cm"

cm4 :: Number -> Number -> Number -> Number -> StyleProperty
cm4 = toUnitWith4 Number.toString "cm"

-- | Milimeters
mm :: Number -> StyleProperty
mm = toUnitWith Number.toString "mm"

mm2 :: Number -> Number -> StyleProperty
mm2 = toUnitWith2 Number.toString "mm"

mm4 :: Number -> Number -> Number -> Number -> StyleProperty
mm4 = toUnitWith4 Number.toString "mm"

-- | Inches (1in ≈ 2.54cm)
inches :: Number -> StyleProperty
inches = toUnitWith Number.toString "in"

inches2 :: Number -> Number -> StyleProperty
inches2 = toUnitWith2 Number.toString "in"

inches4 :: Number -> Number -> Number -> Number -> StyleProperty
inches4 = toUnitWith4 Number.toString "in"

-- | Points (1pt = 1/72 of 1in)
pt :: Number -> StyleProperty
pt = toUnitWith Number.toString "pt"

pt2 :: Number -> Number -> StyleProperty
pt2 = toUnitWith2 Number.toString "pt"

pt4 :: Number -> Number -> Number -> Number -> StyleProperty
pt4 = toUnitWith4 Number.toString "pt"

-- | Picas (1pc = 12 pt)
pc :: Number -> StyleProperty
pc = toUnitWith Number.toString "pc"

pc2 :: Number -> Number -> StyleProperty
pc2 = toUnitWith2 Number.toString "pc"

pc4 :: Number -> Number -> Number -> Number -> StyleProperty
pc4 = toUnitWith4 Number.toString "pc"

-- Relative length units

-- | Relative to the font-size of the element (2em means 2 times the size of
-- | the current font).
em :: Number -> StyleProperty
em = toUnitWith Number.toString "em"

em2 :: Number -> Number -> StyleProperty
em2 = toUnitWith2 Number.toString "em"

em4 :: Number -> Number -> Number -> Number -> StyleProperty
em4 = toUnitWith4 Number.toString "em"

-- | Relative to the x-height of the current font (rarely used).
ex :: Number -> StyleProperty
ex = toUnitWith Number.toString "ex"

ex2 :: Number -> Number -> StyleProperty
ex2 = toUnitWith2 Number.toString "ex"

ex4 :: Number -> Number -> Number -> Number -> StyleProperty
ex4 = toUnitWith4 Number.toString "ex"

-- | Relative to the width of the "0" (zero) character.
ch :: Number -> StyleProperty
ch = toUnitWith Number.toString "ch"

ch2 :: Number -> Number -> StyleProperty
ch2 = toUnitWith2 Number.toString "ch"

ch4 :: Number -> Number -> Number -> Number -> StyleProperty
ch4 = toUnitWith4 Number.toString "ch"

-- | Relative to font-size of the root element.
rem :: Number -> StyleProperty
rem = toUnitWith Number.toString "rem"

rem2 :: Number -> Number -> StyleProperty
rem2 = toUnitWith2 Number.toString "rem"

rem4 :: Number -> Number -> Number -> Number -> StyleProperty
rem4 = toUnitWith4 Number.toString "rem"

-- | Relative to 1% of the width of the viewport.
vw :: Number -> StyleProperty
vw = toUnitWith Number.toString "vw"

vw2 :: Number -> Number -> StyleProperty
vw2 = toUnitWith2 Number.toString "vw"

vw4 :: Number -> Number -> Number -> Number -> StyleProperty
vw4 = toUnitWith4 Number.toString "vw"

-- | Relative to 1% of the height of the viewport.
vh :: Number -> StyleProperty
vh = toUnitWith Number.toString "vh"

vh2 :: Number -> Number -> StyleProperty
vh2 = toUnitWith2 Number.toString "vh"

vh4 :: Number -> Number -> Number -> Number -> StyleProperty
vh4 = toUnitWith4 Number.toString "vh"

-- | Relative to 1% of viewport's smaller dimension.
vmin :: Number -> StyleProperty
vmin = toUnitWith Number.toString "vmin"

vmin2 :: Number -> Number -> StyleProperty
vmin2 = toUnitWith2 Number.toString "vmin"

vmin4 :: Number -> Number -> Number -> Number -> StyleProperty
vmin4 = toUnitWith4 Number.toString "vmin"

-- | Relative to 1% of viewport's larger dimension.
vmax :: Number -> StyleProperty
vmax = toUnitWith Number.toString "vmax"

vmax2 :: Number -> Number -> StyleProperty
vmax2 = toUnitWith2 Number.toString "vmax"

vmax4 :: Number -> Number -> Number -> Number -> StyleProperty
vmax4 = toUnitWith4 Number.toString "vmax"

-- | Relative to the parent element.
percent :: Number -> StyleProperty
percent = toUnitWith Number.toString "%"

percent2 :: Number -> Number -> StyleProperty
percent2 = toUnitWith2 Number.toString "%"

percent4 :: Number -> Number -> Number -> Number -> StyleProperty
percent4 = toUnitWith4 Number.toString "%"

toUnitWith :: forall a. (a -> String) -> String -> a -> StyleProperty
toUnitWith f s a = str (f a <> s)

toUnitWith2 :: forall a. (a -> String) -> String -> a -> a -> StyleProperty
toUnitWith2 f s a b = toUnitWithN f s [a, b]

toUnitWith4 :: forall a. (a -> String) -> String -> a -> a -> a -> a -> StyleProperty
toUnitWith4 f s a b c d = toUnitWithN f s [a, b, c, d]

toUnitWithN :: forall a. (a -> String) -> String -> Array a -> StyleProperty
toUnitWithN f s as = str (String.joinWith " " (map (\a -> f a <> s) as))
