---
title:                "HTML का Parsing"
html_title:           "Haskell: HTML का Parsing"
simple_title:         "HTML का Parsing"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## क्यों

क्या आपने कभी सोचा है कि वेब पेज की हमारी सामान्य तस्वीरें और पाठ कैसे प्रकाशित होती हैं? यह सब टेक्स्ट से होता है! हाँ, आपने सही सुना। एचटीएमएल को प्रक्रिया करने के लिए, हस्केल का उपयोग किया जा सकता है। इस आर्टिकल में, हम आपको हस्केल में एचटीएमएल पार्सिंग के बारे में बताएंगे और आपको कुछ उदाहरण भी देंगे कि फिर विभिन्न पाठ, लिंक और दूसरे तस्वीरों को कैसे प्राप्त किया जा सकता है।

## हास्केल में एचटीएमएल पार्सिंग

चलिए एक साधारण हस्केल फ़ंक्शन से शुरू करते हैं जो HTML पार्सिंग करती है:

```Haskell
parseHTML :: String -> [String]
parseHTML html = betweenTags html
```

जैसा कि आप देख सकते हैं, हमने एक स्ट्रिंग को पारामीटर के रूप में दिया है और HTML का बनावट बनाया है। फिर अंतर की तालिका को वापसी किया है जि‌समें दो स्ट्रिंग अन्दर हैं (इंस्क्रुदर्डिंग स्ट्रिंग या कोमेंटएस)।

यहां आप एक उदाहरण देख सकते हैं:

```Haskell
parseHTML "<html><body><h1>Hello, world!</h1></body></html>"
```
और प्रकाशित पाठ को पेश किया। आप अपेक्षित पाठ को देख सकते हैं:

```Haskell
["Hello, world!"]
```

कुछ इसी प्रकार से, हम आपको लिंक और तस्वीरें प्राप्त करने के लिए एक और फ़ंक्शन प्रस्तुत करते हैं:

```Haskell
extractLinks :: String -> [String]
extractLinks html = filter isLink parsed
    where parsed = parseHTML html
          isLink s = "http://" `isPrefixOf` s || "https://" `isPrefixOf` s
```

इस अंतिम फ़ंक्शन में, हमने एक पूर्व निर्ध