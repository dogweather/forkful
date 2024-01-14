---
title:                "Haskell: Json के साथ काम करना"
simple_title:         "Json के साथ काम करना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## क्यों

JSON हैस्केल में डेटा को प्रसंस्कृत करने का एक प्रचलित तरीका है और यह डेटा संग्रहित करने और उन्हें प्रसारित करने के लिए आसानी से इस्तेमाल किया जा सकता है। यह डेटा की वर्णनात्मक रूपरेखा है और इसे रीड और लिखा जा सकता है।

## कैसे करें

इस सेक्शन में हम आपको JSON को हैस्केल में कैसे कोड करें और उसका आउटपुट कैसे लें, के बारे में दिखाएंगे। हम इसे ```Haskell ...``` कोड ब्लॉक सहित उदाहरणों के साथ देखेंगे।

```Haskell
import Data.Aeson

-- Example JSON data
json = "{\"name\":\"John\", \"age\":30, \"hobbies\":[\"reading\", \"coding\"]}"

-- Parsing JSON data into a Haskell value
value = decode json :: Maybe Value

-- Accessing data from the value using key-value pair
name = value >>= \obj -> obj .: "name" :: Maybe String

-- Output: Just "John"

-- Converting a Haskell value into JSON
newJSON = encode (object [("name" .= "Jane"), ("age" .= 25), ("hobbies" .= ["painting", "singing"])])

-- Output: "{\"name\":\"Jane\",\"age\":25,\"hobbies\":[\"painting\",\"singing\"]}"
```

## गहराई में

JSON हैस्केल में बहुत सारे मान्यांकन रूपों और प्रविष्टियों के साथ काम करें, जो इसे डेटा को प्रस्तुत करने के और उससे इस्तेमाल करने की अनेक संभावनाओं को देते हैं। आप डेटा को सेरीजीअलाइज और डेसेरीलाइज करने, अनुक्रम देने, अनुचित मानों को हैंडल करने और अन्य विषयों को सुलझाने के बारे में और अधिक जान सकते हैं।

## हमें से अधिक देखें

- [JSON डिकोडिंग प्रक्रिया](https://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#g:1)
- [JSON कोडिंग प्रक्रिया](https://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#g:2)
- [Aeson पूर्वेक्षण](https://artyom.me/aeson)
- [Scullery: एक हैस्केल लाइब्रेरी जो सरल और शक्तिशाली JSON