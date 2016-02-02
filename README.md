# Shopify Json Catalogue Parser (product.hs)

Returns the cost of buying as much variations of keyboards and computers, without passing over 100kg.

> costof ( (type = computer | type = keyboard) && sum(grams [purchases] < 100kg) )

### Dependencies

- [http-conduit] 
- [aeson]

[http-conduit]: <https://hackage.haskell.org/package/http-conduit>
[aeson]: <https://hackage.haskell.org/package/aeson>

### Installation

```sh
  $ brew install ghc haskell-platform
  $ cabal install http-conduit aeson
```

### Usage

```sh
  $ ghc -e 'main' product.hs
```