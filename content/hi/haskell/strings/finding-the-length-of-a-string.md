---
title:                "स्ट्रिंग की लंबाई ज्ञात करना"
aliases: - /hi/haskell/finding-the-length-of-a-string.md
date:                  2024-01-20T17:48:20.352273-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग की लंबाई ज्ञात करना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
स्ट्रिंग की लंबाई जानना मतलब यह पता लगाना होता है कि किसी स्ट्रिंग में कितने अक्षर हैं। प्रोग्रामर इसे इसलिए करते हैं ताकि वे डेटा प्रोसेसिंग, इनपुट वैलिडेशन या यूजर इंटरफ़ेस डिज़ाइन में इस जानकारी का इस्तेमाल कर सकें।

## कैसे करें:
Haskell में स्ट्रिंग की लंबाई जानने का सीधा तरीका यहां दिया गया है:

```Haskell
main :: IO ()
main = do
    let myString = "नमस्ते"
    print $ length myString
```

जब आप ऊपर दिया कोड चलाएँगे, तब निकलने वाला आउटपुट होगा:

```
6
```

## गहराई से जानें:

स्ट्रिंग की लंबाई जानने का काम हास्केल में `length` फ़ंक्शन से होता है, जो लिस्ट पर काम करता है क्योंकि हास्केल में स्ट्रिंग एक चर चर की लिस्ट होती है। इतिहास में, ऐसे कई प्रोग्रामिंग मॉडल आए हैं जो स्ट्रिंग ऑपरेशन्स की अवधारणाओं को मानते हैं।

हास्केल में स्ट्रिंग्स इम्यूटेबल होते हैं, जिसका अर्थ है कि एक बार स्ट्रिंग बनाने के बाद, उसमें बदलाव नहीं किए जा सकते हैं। इसकी वजह से `length` फ़ंक्शन का इस्तेमाल करके स्ट्रिंग की लंबाई जानने का काम ज्यादा तर्कसंगत होता है। 

वैकल्पिक तरीके भी हैं, जैसे कि `Data.Text` लाइब्रेरी का इस्तेमाल करना जो UTF-8 एन्कोडिंग को बेहतर ढंग से संभालता है। `Data.Text.length` फ़ंक्शन का इस्तेमाल भी `length` की तरह ही होता है, पर यह `Text` टाइप की स्ट्रिंग्स के साथ काम करता है।

## संबंधित सूत्र:

- [Haskell `length` function documentation](https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#v:length)
- [Haskell `Data.Text` module](https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html)
- [Learn You a Haskell for Great Good! (An accessible book for beginners)](http://learnyouahaskell.com/chapters)
