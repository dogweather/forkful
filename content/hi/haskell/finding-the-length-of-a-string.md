---
title:    "Haskell: स्ट्रिंग की लंबाई का पता लगाना"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## क्यों

भाषा की लंबाई को जानना काफी उपयोगी हो सकता है, जैसे कि विशेष कार्यों के लिए शब्द सीमा, या उपयोगकर्ता के इनपुट को सत्यापित करने के लिए।

## कैसे करें

```
Haskell कोड उदाहरण:
len :: String -> Int
len str = length str
```

**उत्पादन:**
```
len "गर्मी" => 4
len "खुशी" => 3
```

## गहराई में जाएं

भाषा लंबाई को निकालने के लिए `length` फ़ंक्शन का उपयोग होता है, जो `Prelude` मॉड्यूल में पहले से ही उपलब्ध है। यह फ़ंक्शन एक स्ट्रिंग या किसी भी शून्य के समान इलेमेंट की संख्या वापस करता है। तो अगर हमारे पास `len` फ़ंक्शन नहीं है, तो हम `length` को अपनी कस्टम फ़ंक्शन के रूप में भी निर्धारित कर सकते हैं।

## देखें भी

- [Prelude मॉड्यूल का डॉक्यूमेंटेशन](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#v:length)
- [अधिक कस्टम डेटा टाइप कैसे बनाएं](https://www.haskell.org/tutorial/goodies.html#algebraic-data-types)
- [एक स्ट्रिंग में अस्थायी विराम कैसे हटाएं](https://wiki.haskell.org/How_to_remove_a_%22(some_letter)%22_from_a_string)