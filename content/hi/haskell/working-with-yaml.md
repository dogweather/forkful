---
title:                "yaml से काम करना"
html_title:           "Haskell: yaml से काम करना"
simple_title:         "yaml से काम करना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

# क्यों

यामल काम करना बेहतर अनुभव बनाने के लिए है। यह एक आसान और स्वचालित फॉर्मेट है जो डेटा को संरक्षित और संगठित रखने में मदद करता है। यहाँ हिन्दी में हम हैस्केल के साथ यामल पर काम करना सीखेंगे।

# कैसे करें

यामल की सबसे पहले आवश्यकता होती है, तो इसे इंस्टॉल करना है। आप आसानी से अपने सिस्टम में इसे इंस्टॉल कर सकते हैं - ```cabal इंस्टॉल यामल```। इसके बाद, आप हैस्केल में निम्न उदाहरण का पालन करके यामल फ़ाइल में से डेटा पढ़ सकते हैं:

```Haskell
import Data.Yaml

main = do
  fileContents <- decodeFileEither "example.yaml"
  case fileContents of
    Left err -> print err
    Right result -> print result
```

इसका आउटपुट कुछ इस तरह हो सकता है:

```
Right (Object (fromList [("name",String "John"),("age",Number 25)]))
```

आप इसी प्रकार हैस्केल कोड का उपयोग करके यामल डेटा को लिख सकते हैं:

```Haskell
import Data.Yaml

main = do
  let myData = object ["name" .= String "Jane", "age" .= Number 30]
  encodeFile "newfile.yaml" myData
```

यह आपके नए फ़ाइल में डेटा को लिखेगा जिसमें शामिल होता है:

```
name: Jane
age: 30
```

# गहराई में दून

यामल के साथ काम करने के दूराव या तकनीकी विवरण के बारे में और अधिक जानने के लिए, आप हैस्केल परियोजना के अधिकारियों द्वारा संचालित आधिकारिक दस्तावेज़ पढ़ सकते हैं। यामल और हैस्केल दोनों ही ओपन सोर्स परियोजनाओं हैं, तो आप यामल और इस तकनीक को समझने में मदद के लिए अन्य लोगों के संपर्क म