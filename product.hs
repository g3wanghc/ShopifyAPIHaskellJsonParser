{-|
  Project:      Shopify Task
  Description:  (ﾉ◕ヮ◕)ﾉ*:･ﾟ✧ Let's make some additions! ✧ﾟ･: *ヽ(◕ヮ◕ヽ)

  cabal install http-conduit aeson

  Usage: ghc product.hs -e 'main'

  Note: Use homebrew or linuxbrew's brew install ghc haskell-install
        Otherwise, be careful about package dependencies 

  Hanchen Wang 2016
-}

{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

-- Dependencies ----------------------------------------------------------

import Data.Aeson
import Control.Applicative
import Data.Time
import Control.Monad

import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)

import Data.List (sortBy)
import Data.Ord (comparing)

-- Data ------------------------------------------------------------------

jsonURL :: String
jsonURL = "http://shopicruit.myshopify.com/products.json"

getJSON :: IO B.ByteString
getJSON = simpleHttp jsonURL


-- Casting ---------------------------------------------------------------

parseTimeStamp :: String -> Maybe UTCTime
parseTimeStamp = parseTimeM True defaultTimeLocale "%FT%X-%R"

toFloat :: String -> Float
toFloat s = read s :: Float

-- Abstraction -----------------------------------------------------------

data Image = Image {
    image_id            :: Integer,
    image_created_at    :: Maybe UTCTime,
    image_position      :: Integer,
    image_updated_at    :: Maybe UTCTime,
    image_product_id    :: Integer,
    image_variant_ids   :: [Integer],
    image_src           :: String
} deriving (Show)

data Option = Option {
    option_name     :: String,
    option_position :: Integer
} deriving (Show)

data Variant = Variant { 
    variant_id                  :: Integer, 
    variant_title               :: String,
    variant_option1             :: String,
    variant_option2             :: Maybe String,
    variant_option3             :: Maybe String,
    variant_price               :: Float,
    variant_grams               :: Integer,
    variant_compare_at_price    :: Float,
    variant_sku                 :: Maybe String,
    variant_requires_shipping   :: Bool,
    variant_taxable             :: Bool,
    variant_position            :: Integer,
    variant_product_id          :: Integer,
    variant_created_at          :: Maybe UTCTime, 
    variant_updated_at          :: Maybe UTCTime,
    variant_available           :: Bool
} deriving (Show)

data Product = Product {
    product_id              :: Integer,
    product_title           :: String,
    product_handle          :: String,
    product_body_html       :: String,
    product_published_at    :: Maybe UTCTime,
    product_created_at      :: Maybe UTCTime,
    product_updated_at      :: Maybe UTCTime,
    product_vendor          :: String,
    product_product_type    :: String,
    product_tags            :: [String],
    product_variants        :: [Variant],
    product_images          :: [Image],
    product_options         :: [Option]
}deriving (Show)

data Shop = Shop{
    shop_data       :: [Product]
} deriving (Show)

-- Hydrate -------------------------------------------------------------

instance FromJSON Image where
    parseJSON (Object v) = 
        Image <$>
        (v .: "id")                                 <*>
        liftM parseTimeStamp (v .: "created_at")    <*>
        (v .: "position")                           <*>
        liftM parseTimeStamp (v .: "updated_at")    <*>
        (v .: "product_id")                         <*>
        (v .: "variant_ids")                        <*>
        (v .: "src")

instance FromJSON Option where
    parseJSON (Object v) = 
        Option <$>
        (v .: "name")       <*>
        (v .: "position")

instance FromJSON Variant where
    parseJSON (Object v) = 
        Variant <$>
        (v .: "id")                                 <*>
        (v .: "title")                              <*>     
        (v .: "option1")                            <*>
        (v .: "option2")                            <*>
        (v .: "option3")                            <*>
        liftM toFloat (v .: "price")                <*>     
        (v .: "grams")                              <*>
        (v .: "compare_at_price")                   <*>
        (v .: "sku")                                <*>
        (v .: "requires_shipping")                  <*>     
        (v .: "taxable")                            <*>
        (v .: "position")                           <*>
        (v .: "product_id")                         <*>
        liftM parseTimeStamp (v .: "created_at")    <*>     
        liftM parseTimeStamp (v .: "updated_at")    <*>
        (v .: "available")

instance FromJSON Product where
    parseJSON (Object v) = 
        Product <$>
        (v .: "id")                                 <*>
        (v .: "title")                              <*>
        (v .: "handle")                             <*>
        (v .: "body_html")                          <*>
        liftM parseTimeStamp (v .: "published_at")  <*>
        liftM parseTimeStamp (v .: "created_at")    <*>
        liftM parseTimeStamp (v .: "updated_at")    <*>
        (v .: "vendor")                             <*>
        (v .: "product_type")                       <*>
        (v .: "tags")                               <*>
        (v .: "variants")                           <*>
        (v .: "images")                             <*>
        (v .: "options")

instance FromJSON Shop where
    parseJSON (Object v) = 
        Shop <$>
        (v .: "products") 

-- IO ------------------------------------------------------------------

tryGet = (eitherDecode <$> getJSON) :: IO (Either String Shop)

-- Logic ---------------------------------------------------------------


matchesTypes :: String -> [String] -> Bool
matchesTypes product_type types = (elem product_type types)

matchedProducts :: [Product] -> [String] -> [Product]
matchedProducts products types = 
    filter (\x -> (matchesTypes (product_product_type x) types) ) products
 
matchedVariants :: Shop -> [String] -> [Variant]
matchedVariants catalogue types = (foldr (++) [] (map product_variants (
    matchedProducts (shop_data catalogue) types)))

-- greedy approach
orderVariantByGrams :: [Variant] -> [Variant]
orderVariantByGrams = sortBy (comparing variant_grams)

availableVariants :: [Variant] -> [Variant]
availableVariants variants = filter variant_available variants

rollingSumVariants :: [Variant] -> [(Integer, Variant)]
rollingSumVariants variants = zip (scanl1 (+) (map variant_grams variants)) variants

carryAsMuch :: [Variant] -> Integer -> [Variant]
carryAsMuch variants strength = (map snd 
    (takeWhile (\(c_sum, variant) -> c_sum <= strength) (rollingSumVariants variants)))

numberOfOptions :: Variant -> Float
numberOfOptions variant = 1

costOfVariants :: [Variant] -> Float
costOfVariants variants = foldr (+) 0 (
    map (\x -> (variant_price x) * (numberOfOptions x)) variants)

totalCost :: Shop -> [String] -> Integer -> Float
totalCost catalogue types strenght = (
    costOfVariants (carryAsMuch (
        matchedVariants catalogue types) 
    strenght))

carry_strenght_grams = 100000
desired_item_types = ["Computer", "Keyboard"]

-- Main

main :: IO ()
main = do 
    result <- tryGet 
    case result of 
        Left err    -> putStrLn ("Something went wrong: " ++ err)
        Right shop_catalogue -> putStrLn ("It will cost: $" 
            ++ show (totalCost shop_catalogue desired_item_types carry_strenght_grams)
            )