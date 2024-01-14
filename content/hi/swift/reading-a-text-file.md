---
title:                "Swift: एक टेक्स्ट फाइल को पढ़ना"
simple_title:         "एक टेक्स्ट फाइल को पढ़ना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## क्यों 

वैसे तो Swift में बहुत से फंक्शन हैं जो हमें फाइल से डेटा पढ़ने में मदद कर सकते हैं, लेकिन "text file" से डेटा पढ़ने की प्रक्रिया थोड़ी अलग है। इस लेख में हम जानेंगे कि क्यों और कैसे हम Swift में एक टेक्स्ट फ़ाइल से डेटा पढ़ सकते हैं।

## कैसे करें 

तो शुरुआत में हमारे पास एक फ़ाइल होगी "text.txt" जो हमें पढ़नी है। उसके बाद हम एक "do-catch" ब्लॉक में "FileManager" के "default" प्रॉपर्टी को उस फ़ाइल का पथ देकर कोल करते हैं। फिर हम एक "if let" के साथ "String" का इंस्टेंस बनाते हैं जो हमें उस फ़ाइल से डेटा पढ़ने में मदद करेगा। और अंत में हम उस "String" को प्रिंट करते हैं। यह सब दिखाने के लिए, नीचे दिए गए उदाहरण को देखें:

```Swift
do {
    let fileManager = FileManager.default
    let path = "/Users/User/Desktop/text.txt"
    if let text = try String(contentsOf: URL(fileURLWithPath: path), encoding: .utf8) {
        print(text)
    }
} catch {
    print("Couldn't read the file")
}
```

जैसा कि आप देख सकते हैं, हमने "try" और "catch" का इस्तेमाल किया है ताकि हमें एरर मेसेज को प्रिंट करने का मौका मिले अगर कुछ गलत हुआ होता है। और अगर सब कुछ सही रहता है, तो हमें उस फ़ाइल का डेटा मिल जाता है जो हम अपने "text" वेरिएबल में स्टोर करते हैं और फिर उसे प्रिंट करते हैं। फ़ाइल पढ़ने की प्रक्रिया सरल है और हम देखेंगे कि अगर हमारे पास कोई अन्य प्रकार की फ़ाइल होती तो भी हम उसे पढ़ सकते हैं उसी तरह से।

## गहराई