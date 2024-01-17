---
title:                "उप-शब्दों को निकालना"
html_title:           "Fish Shell: उप-शब्दों को निकालना"
simple_title:         "उप-शब्दों को निकालना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

अभिकल्पना बहुत सारे जांच-संशोधन समायोजनों के लिए है - बस स्ट्रिंग के कुछ हिस्से को निर्दिष्ट करने से प्रत्येक एक जाच को अपने स्तर पर लागू करने के लिए इस्तेमाल किया जा सकता है। स्ट्रिंग कोड साथ डेटा को पारस्परिक रूप से जोड़ा जा सकता है जब चाहे हो तो।

## कैसे:

```Fish Shell अॅगुभराउंड```

```
set str Hello World!
echo $str[5]
```

आउटपुट:
```
o
```

```Fish Shell अॅगुभराउंड```

```
set num 123456789
echo $num[2..4]
```

आउटपुट:
```
234
```

## डीप डाइव:

1. इस तकनीक की उत्पत्ति बहुत पुरानी है, CUDA और OpenMP इसका उपयोग करने के लिए बहुत प्रसिद्ध है ।
2. दूसरी तकनीक से विभिन्न काम किया जा सकता है, जैसे कि फुंक्शन्स को इस्तेमाल करके निर्दिष्ट स्त्रिंग का उत्पादन करना।
3. यह तकनीक एक स्ट्रिंग के हर एक अक्षर को अलग करती है, ताकि कोई भी इंटर्नल लोजिक उस पर इम्प्लिमेंट किया जा सके।

## सी उल्लंघन:

स्ट्रिंग के साथ काम करने का एक शानदार कॉम्बिनेशन ```Fish Shell``` है। और हमारे [इस लेख] (https://github.com/fish-shell/fish-shell/blob/master/doc_src/index.md) का उपयोग करके आप इस समस्या को गंभीरता से हल कर सकते हैं।

## देखें भी:

- [Fish Shell क्या है] (https://fishshell.com/)
- [C भाषा सीखें] (https://www.learn-c.org/)
- [जांच-संशोधन समायोजन के लिए संबंधित स्रोतें] (https://github.com/search?q=substring+extractor&type=Repositories)