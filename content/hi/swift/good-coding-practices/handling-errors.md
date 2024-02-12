---
title:                "एरर्स को हैंडल करना"
aliases: - /hi/swift/handling-errors.md
date:                  2024-01-26T01:00:27.363006-07:00
model:                 gpt-4-1106-preview
simple_title:         "एरर्स को हैंडल करना"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/handling-errors.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
स्विफ्ट में त्रुटियों को हैंडल करने का अर्थ है कि आपका कोड चलते समय अगर कोई समस्याएं आती हैं, तो उनका अनुमान लगाना और प्रतिक्रिया देना। हम इसे अराजकता पर नियंत्रण रखने के लिए करते हैं—ऐप्स के क्रैश होने से रोकने और उपयोगकर्ता को सुचारू अनुभव देने के लिए।

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
