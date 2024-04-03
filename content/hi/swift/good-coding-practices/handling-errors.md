---
date: 2024-01-26 01:00:27.363006-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: \u0938\u094D\u0935\
  \u093F\u092B\u094D\u091F `do`, `try`, \u0914\u0930 `catch` \u092C\u094D\u0932\u0949\
  \u0915 \u0915\u0947 \u0938\u093E\u0925 \u0924\u094D\u0930\u0941\u091F\u093F \u0939\
  \u0948\u0902\u0921\u0932\u093F\u0902\u0917 \u0915\u093E \u0907\u0938\u094D\u0924\
  \u0947\u092E\u093E\u0932 \u0915\u0930\u0924\u093E \u0939\u0948\u0964 \u091A\u0932\
  \u093F\u090F \u0926\u0947\u0916\u0924\u0947 \u0939\u0948\u0902."
lastmod: '2024-03-13T22:44:52.931113-06:00'
model: gpt-4-1106-preview
summary: "\u0938\u094D\u0935\u093F\u092B\u094D\u091F `do`, `try`, \u0914\u0930 `catch`\
  \ \u092C\u094D\u0932\u0949\u0915 \u0915\u0947 \u0938\u093E\u0925 \u0924\u094D\u0930\
  \u0941\u091F\u093F \u0939\u0948\u0902\u0921\u0932\u093F\u0902\u0917 \u0915\u093E\
  \ \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u0930\u0924\u093E \u0939\
  \u0948\u0964 \u091A\u0932\u093F\u090F \u0926\u0947\u0916\u0924\u0947 \u0939\u0948\
  \u0902."
title: "\u090F\u0930\u0930\u094D\u0938 \u0915\u094B \u0939\u0948\u0902\u0921\u0932\
  \ \u0915\u0930\u0928\u093E"
weight: 16
---

## कैसे करें:
स्विफ्ट `do`, `try`, और `catch` ब्लॉक के साथ त्रुटि हैंडलिंग का इस्तेमाल करता है। चलिए देखते हैं:

```Swift
enum FileError: Error {
    case fileDoesNotExist
    case noPermission
}

func readFile(atPath path: String) throws -> String {
    // मान लो हमारे पास यहाँ कुछ तर्क है जो जांचता है कि फाइल मौजूद है और क्या हमें इसे पढ़ने की अनुमति है
    let fileExists = false
    let havePermission = true

    if !fileExists {
        throw FileError.fileDoesNotExist
    }

    if !havePermission {
        throw FileError.noPermission
    }

    return "फाइल की सामग्री यहाँ जाती है"
}

do {
    let fileContent = try readFile(atPath: "/path/to/file")
    print(fileContent)
} catch FileError.fileDoesNotExist {
    print("ओहो! फाइल नहीं मिली।")
} catch FileError.noPermission {
    print("अहा! फाइल पढ़ने की अनुमति नहीं है।")
} catch {
    print("एक अज्ञात त्रुटि हुई।")
}

```

नमूना आउटपुट:

```
ओहो! फाइल नहीं मिली।
```

## गहराई में जानें
त्रुटि हैंडलिंग हमेशा इतनी सुचारू नहीं थी जैसी अब है। ऑब्जेक्टिव-सी में, आप NSError ऑब्जेक्ट्स के पॉइंटर्स के साथ डील करते थे, जो थोड़ा अजीब लगता था। अब, हमारे पास स्विफ्ट एनम्स और `Error` प्रोटोकॉल के साथ एक अधिक सुरुचिपूर्ण प्रणाली है।

स्विफ्ट का `throw` हमें यह संकेत करने देता है कि कुछ गड़बड़ हो गई है। `do` ब्लॉक्स त्रुटि-सजग क्षेत्रों की तरह काम करते हैं, `try` जोखिम भरे कामों को बुलाते हैं, और `catch` चीजों को संभालते हैं अगर वे गलत हो जाएं।

ऑप्शनल्स उन स्थितियों के लिए एक वैकल्पिक होते हैं जो काफी "त्रुटि" स्थिति नहीं हैं लेकिन फिर भी "कोई परिणाम नहीं" हो सकता है। यह थोड़ा श्रोडिंगर के वरिएबल्स जैसा है—उनका कोई मूल्य होता है या नहीं होता है।

वास्तविक गहराई के लिए, `Result` प्रकारों की जांच करें, जो नियमित-रिटर्न और त्रुटि पैटर्न के बीच स्मार्ट हाइब्रिड होते हैं।

## यह भी देखें
- आधिकारिक स्विफ्ट त्रुटि हैंडलिंग गाइड: [Apple Docs](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html)
- स्विफ्ट त्रुटि हैंडलिंग बेस्ट प्रैक्टिसेज: [RayWenderlich.com](https://www.raywenderlich.com/1851-beginning-swift-error-handling)
- स्विफ्ट में एडवांस त्रुटि हैंडलिंग: [Medium Article](https://medium.com/better-programming/advanced-error-handling-in-swift-4f6bdf6b01d8)
