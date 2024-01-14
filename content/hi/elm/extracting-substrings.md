---
title:                "Elm: उपस्थित स्ट्रिंग्स निकालना"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

# क्यों

कभी-कभी हम एक लचीली स्ट्रिंग से एक छोटी सी प्रतिलिपि निकालना चाहते हैं, जैसे कि बिस्तर सफ़ेद नामों के लिए केवल पहला नाम दिखाना। यहाँ, Elm में सबसे आसान तरीका है स्ट्रिंग को स्प्लिट करके उससे आवश्यक तत्वों को चुनना।

# कैसे करें

यदि हमें बिस्तर सफ़ेद नामों के लिए केवल पहला नाम चाहिए हो, तो हम `String.split` विधि का उपयोग कर सकते हैं। यह विधि एक धागा और स्प्लिटर को ग्रहण करती है और स्लाइस में भेजती है, जो एक शून्य से एक कम होता है।

```Elm
String.split " " "John Doe"           -- ["John", "Doe"]
String.split "." "www.example.com"    -- ["www", "example", "com"]
```

इसके बाद हम `List.head` विधि का उपयोग करके पहली स्ट्रिंग आउटपुट में लाएंगे।

```Elm
List.head (String.split " " "John Doe")    -- "John"
```

# गहराई में

कभी-कभी हम अपनी स्ट्रिंग का सेकंड या तीसरा स्लाइस भी चुनना चाहते हैं, जैसे "www.example.com" से "example" लाना। इसके लिए हम `List.Extra` मॉड्यूल के साथ `List.getAt` विधि का उपयोग करेंगे।

```Elm
List.getAt 1 (String.split "." "www.example.com")    -- "example"
```

यदि हमें स्लाइस का उपयोग करके एक अन्य विशिष्ट शंकुनिय बनाना हो, तो हम `String.slice` विधि का उपयोग करके कोई विशिष्ट स्थान से शुरू कर सकते हैं।

```Elm
String.slice 4 6 "ABCDEFGHIJ"    -- "EF"
```


# देखें भी

- [Elm डॉक्युमेंटेशन](https://package.elm-lang.org/packages/elm/core/1.0.0/String) 
- [अधिक तकनीकी सूचना के लिए Elm फोरम](https://discourse.elm-lang.org/)