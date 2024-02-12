---
title:                "लॉगिंग"
aliases: - /hi/swift/logging.md
date:                  2024-01-26T01:08:55.379886-07:00
model:                 gpt-4-1106-preview
simple_title:         "लॉगिंग"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/logging.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
लॉगिंग एप्लिकेशन के व्यवहार, त्रुटियों, और अन्य महत्वपूर्ण जानकारी को एक स्थायी माध्यम, जैसे एक फ़ाइल या डेटाबेस पर रिकॉर्ड करने की प्रक्रिया है। प्रोग्रामर इसे अपने एप्स की स्वास्थ्य और प्रदर्शन को ट्रैक करने, समस्याओं को डीबग करने, और उत्पादन पर्यावरणों में हुड के नीचे क्या हो रहा है, इस पर नजर रखने के लिए करते हैं।

## कैसे करें:
Swift में, आप "print" स्टेटमेंट्स के साथ या अधिक लचीले `os.log` API के साथ कंसोल पर लॉग लिख सकते हैं, जो Apple प्लेटफ़ॉर्म्स पर Unified Logging System में जुड़ता है।

```Swift
import os.log

let logger = OSLog(subsystem: "com.yourapp.domain", category: "network")

func fetchData() {
    // सरल print स्टेटमेंट
    print("Fetch started")
    
    // os.log का उपयोग करते हुए info-level इवेंट की लॉगिंग
    os_log(.info, log: logger, "Fetching data from API.")
    
    do {
        let data = try performNetworkRequest()
        // debug-level इवेंट की लॉगिंग
        os_log(.debug, log: logger, "Data received: %@", data.description)
    } catch {
        // error-level इवेंट की लॉगिंग
        os_log(.error, log: logger, "Failed to fetch data: %@", error.localizedDescription)
    }
}

func performNetworkRequest() throws -> Data {
    // नेटवर्क रिक्वेस्ट का अनुकरण
    return Data()
}
```

कंसोल पर ओउटपुट कुछ इस तरह दिख सकता है:

```
Fetch started
Fetching data from API.
Data received: Some data bytes...
```

त्रुटियों के लिए, यह हो सकता है:

```
Failed to fetch data: The Internet connection appears to be offline.
```

## गहराई से समझें
iOS 10 और macOS Sierra में पेश किये गए Unified Logging System के साथ Swift में लॉगिंग नई शक्ति और कार्यक्षमता लेकर आती है। `print` स्टेटमेंट के विपरीत जो सीधे कंसोल पर जाता है, यह सिस्टम गतिविधि-आधारित है, और आपको उनके महत्व और वे डीबग या रिलीज़ बिल्ड्स हैं इस आधार पर लॉग संदेशों को फ़िल्टर करने की अनुमति देता है।

ऐतिहासिक संदर्भ iOS और macOS में लॉगिंग के विकास को सामान्य print स्टेटमेंट्स से इंस्ट्रुमेंट्स एप्प और कंसोल के साथ एकीकृत करने वाले व्यापक उपकरणों की ओर दिखाता है, जो सोफ़िस्टिकेट तरीकों से लॉग्स का विश्लेषण करने के लिए प्रदान करते हैं।

Swift में लॉगिंग के विकल्पों के एक श्रेणी मौजूद हैं, जैसे कि CocoaLumberjack जैसे थर्ड-पार्टी लाइब्रेरियां, जो Unified Logging System के ऊपर एक मैक्रो लेयर प्रदान करती हैं। यह लॉग फार्मेटिंग, फाइल प्रबंधन, और प्रदर्शन विकल्पों पर बढ़ा हुआ नियंत्रण प्रदान करती है।

अंत में, कार्यान्वयन विवरण; OSLog का डिजाईन न केवल कार्यक्षमता के लिए है, बल्कि गोपनीयता के प्रति सचेत भी है, लॉगिंग के समय निजी डेटा को धुंधला करने की क्षमता के साथ। यह लॉग्स को फॉल्ट, त्रुटि, जानकारी, और डीबग स्तरों में वर्गीकृत करता है, प्रत्येक ट्रबलशूटिंग के लिए एक अलग अनाज की पेशकश करता है।

## यह भी देखें
- [Apple का Unified Logging दस्तावेज़ीकरण](https://developer.apple.com/documentation/os/logging)
- [Ray Wenderlich का Logging ट्यूटोरियल](https://www.raywenderlich.com/605079-logging-in-swift-oslog)
- [CocoaLumberjack GitHub रिपॉजिटरी](https://github.com/CocoaLumberjack/CocoaLumberjack)
