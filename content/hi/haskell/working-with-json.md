---
title:                "कंप्यूटर प्रोग्रामिंग में json के साथ काम करना"
html_title:           "Haskell: कंप्यूटर प्रोग्रामिंग में json के साथ काम करना"
simple_title:         "कंप्यूटर प्रोग्रामिंग में json के साथ काम करना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
JSON का साथ देना शब्दांतरित करने के लायक है, यह इस्पेल्रश्यन्स के रूप में लोगों के डेटा बदलने की आसान प्रक्रिया है। कई प्रोग्रामर अपनी एप्लिकेशनों में डेटा को संस्करण और समझने के लिए इसका उपयोग करते हैं।

## कैसे करें:
```Haskell
data Person = Person String Int       -- data type of Person with fields Name (String) and Age (Int)

instance FromJSON Person where        -- implementation FromJSON type class
  parseJSON (Object v) = Person <$>   -- get Person value
                         v .: "name" <*>
                         v .: "age"   -- parse name and age fields
  parseJSON _ = empty                 -- error if not a JSON object

decodePerson :: ByteString -> Person  -- decode function for Person type
decodePerson = fromMaybe (Person "" 0) . decode  -- performs actual decoding

-- sample input and output
sampleInput :: ByteString  -- {"name": "John", "age": 25}
sampleOutput :: Person     -- Person "John" 25
sampleOutput = decodePerson sampleInput
```

## गहराई में जाएँ:
JSON का विकास दो शब्दों से होता है - "JavaScript" और "Object Notation"। यह अमेरिकी कम्पनी ने व्हाइटस्पेस के द्वारा नौ शब्दों को उत्पादित किया था। JSON को एक आसान बनाने के लिए, यह उन्हें समान विधि में दिखाता है जो XML इस्तेमाल करते हैं। उन्नियोजित खाली स्थान और खाली स्थान घर्षण के कारण, XML के अनुप्रयोग कई अजमा से अधिक हो सकते हैं। इस स्थिति में, JSON समीना बहुत अधिक कुशलता और कमपीक्षय फाइल आकार में सबसे अधिक उपयोग में आता है।

## इसके अलावा देखें:
[एमएस डॉस](https://msdn.microsoft.com/en-us/library/system.json.jsonobject.aspx), [जावा डॉस](https://docs.oracle.com/javaee/7/api/javax/json/JsonObject.html), [पायथन डॉस](https://docs.python.org/3/library/json.html)। और भी अनेक उदाहरण डॉसियज वाले हैं।