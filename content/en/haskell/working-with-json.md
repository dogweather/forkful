---
title:                "Working with json"
html_title:           "Haskell recipe: Working with json"
simple_title:         "Working with json"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/working-with-json.md"
---

{{< edit_this_page >}}

### What & Why?

JSON (JavaScript Object Notation) is a lightweight data-interchange format that is commonly used for web communication and data storage. It's a simplified way of organizing and storing data that is human-readable and easy for computers to parse. Programmers use JSON because it allows for efficient data transfer between different systems and programming languages.

### How to:

To work with JSON in Haskell, we can use the `aeson` library. First, we need to import the necessary modules:

```Haskell
import Data.Aeson (encode, decode)
import Data.ByteString.Lazy.Char8 (pack, unpack)
```

Next, we can create a simple JSON object using the `Object` data constructor and `fromList` function from the `Data.HashMap.Strict` module:

```Haskell
import qualified Data.HashMap.Strict as HM
let json = Object (HM.fromList [("name",String "John"), ("age", Number 25), ("hobbies", Array [String "hiking", String "reading", String "coding"])])
```

We can then encode this JSON object into a bytestring using the `encode` function and print it out:

```Haskell
let bytestring = encode json
putStrLn (unpack bytestring)
```

The output will be:

```
{"name":"John","age":25,"hobbies":["hiking","reading","coding"]}
```

To decode a JSON bytestring back into a Haskell data structure, we can use the `decode` function:

```Haskell
let decoded = decode bytestring :: Maybe Value
```

In this case, `decoded` will be of type `Maybe Value`, where `Maybe` is used to handle cases where the decoding fails. We can then pattern match and access the values:

```Haskell
case decoded of
  Just (Object obj) -> print (obj HM.! "name")
  _ -> print "Decoding failed."
```

The output will be:

```
"John"
```

### Deep Dive:

JSON was initially developed by Douglas Crockford in the early 2000s. Its simplicity and ability to work well with JavaScript made it popular for web development. However, it has now become a standard for data transfer and is widely used in other programming languages as well.

There are other alternatives to JSON such as XML and YAML. However, JSON's lightweight and simple syntax make it popular for web programming and API development.

In Haskell, the `aeson` library uses the `Data.Aeson` module to encode and decode JSON data. It also provides helpful functions for working with JSON, such as `fromList` and `Object`. Additionally, the `Data.ByteString.Lazy.Char8` module is used to handle bytestrings, which are more efficient for large data structures.

### See Also:

- [Hackage: aeson library](https://hackage.haskell.org/package/aeson)
- [Official JSON website](https://www.json.org/)
- [Douglas Crockford's blog on JSON](https://www.crockford.com/mckeeman.html)