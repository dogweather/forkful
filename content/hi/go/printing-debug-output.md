---
title:                "Go: डिबग उत्पादन प्रिंट करना"
programming_language: "Go"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## क्यों

यदि आप गो प्रोग्रामिंग करते हैं, तो डिबग आउटपुट प्रिंट करना आपके कोड को समझने और बग्स को खोजने में मदद कर सकता है।

## कैसे करें

अगर आप डिबग आउटपुट प्रिंट करना चाहते हैं, तो आपको `fmt.Println()` फ़ंक्शन का उपयोग करना होगा। नीचे दिए गए उदाहरण में, हमने एक स्ट्रिंग को प्रिंट किया है।

```Go
fmt.Println("मेरा पहला गो प्रोग्राम")
```

यह आउटपुट नीचे दिए गए चित्र में दर्शाया गया है:

![प्रिंट किया गया आउटपुट](https://drive.google.com/uc?id=1E59Qm1bb2uZRvkUjCaC18sOgkLQ66uQV)

## गहराई में जायें

आप डिबग आउटपुट को अधिक रूपांतरित करने के लिए `fmt.Printf()` फ़ंक्शन का भी उपयोग कर सकते हैं। यह फ़ंक्शन C में `printf` के समान है। साथ ही, आप अपने कोड में `fmt.Sprintf()` फ़ंक्शन का भी उपयोग कर सकते हैं जो एक स्ट्रिंग को रिटर्न करता है लेकिन उसे प्रिंट नहीं करता है।

आप अपने डिबग आउटपुट को रंग भी दे सकते हैं जैसे कि `fmt.Printf("\033[31m")` जो लाल रंग को दर्शाता है। साथ ही, आप फ़ॉर्मेटिंग चरित्रों का भी उपयोग कर सकते हैं जैसे `%v` या `%T` आदि।

## इसे भी देखें

- [गो की आधिकारिक वेबसाइट](https://golang.org/)
- [गो कोडिंग स्टाइल गाइड](https://github.com/golang/go/wiki/CodeReviewComments)
- [गो साधनें और कतारें](https://github.com/golang/go/wiki/IDEsAndTextEditorPlugins)