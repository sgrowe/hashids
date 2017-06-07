# Hashids

An implementation of the [Hashids](http://hashids.org/) algorithm in Haskell, with a friendly api.

```

> import Hashids

> let (Just (encode, decode)) = hashids defaultHashidSettings

> encode [1,2,3]
"o2fXhV"

> decode "o2fXhV"
[1,2,3]

```

