---
title:                "डीबग आउटपुट प्रिंट करना"
html_title:           "Gleam: डीबग आउटपुट प्रिंट करना"
simple_title:         "डीबग आउटपुट प्रिंट करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)

डीबग आउटपुट प्रिंट करना, मूल रूप से, एक प्रोग्राम का विकल्प होता है जो उसकी क्रियाकलापों की जांच करने में मदद करता है। प्रोग्रामर्स यह करते हैं ताकि वे समस्याओं को खोज सकें और कोड की कार्यकारीता को बेहतर देख सकें। 

## कैसे : (How To:)

Swift में, डीबग आउटपुट `print` फ़ंक्शन का उपयोग करने से प्रिंट किया जा सकता है:

``` Swift
let name = "Swift"
print("Hello, \(name)")
```

उपरोक्त कोड का आउटपुट होगा:

```
Hello, Swift
```

## गहरा डाइव (Deep Dive)

1. ऐतिहासिक प्रसंग: 

   'print' फ़ंक्शन का उपयोग करने की सलाह 1960 की भाषाओं से होती आई है, जैसे कि FORTRAN। Swift ने इसे भी अपना लिया है।

2. विकल्प: 

   Swift में, डीबग प्रिंट (`debugPrint`) और डीबग लुग (`debugLog`) फ़ंक्शन भी होते हैं, जिन्हें भी उपयोग कर सकते हैं। 

3. विन्यास विवरण:

   `print` फ़ंक्शन debug output के लिए व्यवस्थागत टूल है और इसे आमतौर पर रिलीज़ बिल्ड में नहीं शामिल किया जाता है। 

## देखें भी (See Also)

1. [Apple's Swift Documentation on Console Output](https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/TheBasics.html)
2. [Swift Debugging Guide](https://developer.apple.com/library/archive/documentation/DeveloperTools/Conceptual/debugging_with_xcode/chapters/special_debugging_workflows.html)