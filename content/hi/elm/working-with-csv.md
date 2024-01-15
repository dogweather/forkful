---
title:                "कंप्यूटर प्रोग्रामिंग में सीएसवी से काम करना"
html_title:           "Elm: कंप्यूटर प्रोग्रामिंग में सीएसवी से काम करना"
simple_title:         "कंप्यूटर प्रोग्रामिंग में सीएसवी से काम करना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## Kyu
CSV (Comma Separated Values) ek common data format hai jiska istemal data store aur exchange karne ke liye kiya jata hai. Elm mein CSV ka sath lena aasan aur efficient hai, jo file handling aur data manipulation ko asan banata hai.

## Kaise Kare
Elm mein CSV file ka sath lena bohot hi easy hai. Sabse pehle, aapko `elm-explorations/csv` package ko import karna hoga. Fir, CSV file ko decode karne ke liye `Decode.field` function ka istemal kiya jata hai. Yeh function ek FieldParser ko input leta hai aur CSV string ko decode karta hai. Iske baad, decoded data ko desired format mein convert kar sakte hai.

```Elm
import Csv exposing (FieldParser, Decode)
import Csv.Decode exposing (field)

myCsvFile : String
myCsvFile = """
Name, Age
John, 25
Emily, 30
"""

myDecoder : FieldParser (List User)
myDecoder =
  Decode.map2 User
    (field "Name" Decode.string)
    (field "Age" Decode.int)

type alias User = 
  { name : String
  , age : Int
  }

decodedData : Result String (List User)
decodedData =
  Csv.Decode.decode myDecoder myCsvFile
  -- Result is Ok [{ name = "John", age = 25 }, { name = "Emily", age = 30 }]
```

## Deep Dive
Elm mein CSV ko sambhalne ka kaam "elm-explorations/csv" package se hoga. Is package ke dwara provided functions CSV file ko decode aur encode karne mein madad karti hai. Package ke functions, data ko convert karna, data types ko define karna, aur CSV headers ko handle karna mein help karte hai. Yeh package developers ko complex data handling se bachata hai aur ashan aur efficient tarike se CSV files ko handle karne mein help karta hai.

## Dekhe Bhi
- [Elm CSV package documentation](https://package.elm-lang.org/packages/elm-explorations/csv/latest/)
- [Explaining CSV files](https://www.howtogeek.com/348960/what-is-a-csv-file-and-how-do-i-open-it/)
- [Working with CSV files in Elm](https://dev.to/tuata-restrepo/working-with-csv-files-in-elm-cng)