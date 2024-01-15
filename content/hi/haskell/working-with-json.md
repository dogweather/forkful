---
title:                "Json के साथ काम करना"
html_title:           "Haskell: Json के साथ काम करना"
simple_title:         "Json के साथ काम करना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## क्यों

JSON (Javascript Object Notation) एक प्रसिद्ध डेटा संरचना है जो एक संज्ञाश्रुत संघ्रहालय (schema-less) फॉर्मेट में होती है। यह एक Newtonsoft Json एक चुनिंदा उदाहरण है जिससे C# एवं Java के कारकों और Json संरचनाओं का उपयोग होता है। हास्केल में JSON का उपयोग सुगमता से किया जा सकता है जिससे आप अपने हास्केल एप्लीकेशन में डेटा को प्रसन्न उपयोग कर सकते हैं।

## कैसे करें

हास्केल में JSON पर काम करने के लिए आपको हास्केल की Json पुस्तकें (libraries) को इंस्टॉल करने की आवश्यकता होती है। नीचे दिए गए कोड उदाहरण में, हमने अदोनिस (Aeson) पुस्तकी उपयोग किया है जो हास्केल में सबसे अधिक प्रचलित है। यहां, हम सामान्य जेसन के साथ क्षुधाभोजी साइगर संरचनाओं का प्रबंधन करना सिखाएंगे।

```Haskell
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import GHC.Generics (Generic)

-- बिना डेटा के Json संरचना बनाएं
data Tiger = Tiger { name :: String, age :: Int } deriving (Show, Generic)

-- FromJSON और ToJSON क्षुधाभोजी साइगर को जेसन में सीरीलाइज़ और डी-सीरीलाइज़ करते हैं
instance FromJSON Tiger
instance ToJSON Tiger

-- कुछ उदाहरण साइगर बनाएं
tony :: Tiger
tony = Tiger "Tony" 9
shereKhan :: Tiger
shereKhan = Tiger "Shere Khan" 20

-- साइगर। जेसन में सीरीलाइज़ करना
main = do
  let jsonTony = encode tony
  let jsonShereKhan = encode shereKhan
  putStrLn jsonTony -- {"name":"Tony","age":9}
  putStrLn jsonShereKhan --{"name":"Shere Khan","age":20}
```

## गहराई में जाएं

जेसन आपके हास्केल एप्लीकेशन में सीमित से सीमित नहीं होता है, और आप उसे मिश्रित डेटा के साथ भी उपयोग कर