---
date: 2024-01-20 18:05:05.077244-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Xcode \u092E\
  \u0947\u0902 \u0928\u092F\u093E Swift \u092A\u094D\u0930\u094B\u091C\u0947\u0915\
  \u094D\u091F \u092C\u0928\u093E\u0928\u093E \u0906\u0938\u093E\u0928 \u0939\u0948\
  : 1. Xcode \u0916\u094B\u0932\u0947\u0902\u0964 2. 'File' \u092E\u0947\u0928\u094D\
  \u092F\u0942 \u0938\u0947 'New' \u092B\u093F\u0930 'Project...' \u091A\u0941\u0928\
  \u0947\u0902\u0964 3. Templates \u092E\u0947\u0902 \u0938\u0947 \u090F\u0915\u2026"
lastmod: '2024-03-13T22:44:52.919422-06:00'
model: gpt-4-1106-preview
summary: "Xcode \u092E\u0947\u0902 \u0928\u092F\u093E Swift \u092A\u094D\u0930\u094B\
  \u091C\u0947\u0915\u094D\u091F \u092C\u0928\u093E\u0928\u093E \u0906\u0938\u093E\
  \u0928 \u0939\u0948."
title: "\u0928\u0908 \u092A\u0930\u093F\u092F\u094B\u091C\u0928\u093E \u0936\u0941\
  \u0930\u0942 \u0915\u0930\u0928\u093E"
weight: 1
---

## How to: (कैसे करें:)
Xcode में नया Swift प्रोजेक्ट बनाना आसान है:

1. Xcode खोलें।
2. 'File' मेन्यू से 'New' फिर 'Project...' चुनें।
3. Templates में से एक को चुनें, जैसे कि 'Single View App'।
4. प्रोजेक्ट डीटेल्स भरें: नाम, टीम, आइडेंटिफायर, और भाषा।
5. 'Next' बटन पे क्लिक करके अपने प्रोजेक्ट के लिए लोकेशन चुनें और 'Create' पे क्लिक करें।

यहाँ एक सिंपल 'Hello, World!' प्रोग्राम का कोड है:

```Swift
import SwiftUI

struct ContentView: View {
    var body: some View {
        Text("Hello, World!")
            .padding()
    }
}

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
```

इसे रन करते ही, आपको सिम्युलेटर या डिवाइस पे "Hello, World!" का मैसेज दिखाई देगा।

## Deep Dive (गहराई से जानकारी)
Swift की शुरुआत 2014 में Apple ने की थी। Objective-C की तुलना में, Swift ज्यादा सुरक्षित और आसान बनी हुई है, जिससे नए प्रोग्रामर्स के लिए भी इसे समझना और इस्तेमाल करना सरल हो जाता है। प्रोजेक्ट शुरू करने के लिए अलग-अलग टेम्प्लेट्स और बोइलरप्लेट कोड मौजूद हैं जो आधारभूत सेटअप में मदद करते हैं। 

पहले प्रोग्रामर्स Objective-C या अन्य भाषाओं जैसे कि C++ या Python से iOS डेवलपमेंट के लिए शिफ्ट होते थे, पर अब Swift के आने से यह सीखना और आगे बढ़ना ज्यादा सहज हो गया है। Xcode के टूल्स और SwiftUI जैसी टेक्नोलॉजीस से यूजर इंटरफ़ेस का निर्माण भी सरल हो गया है, जो रियल टाइम प्रीव्यू और डेक्लेरेटिव सिंटेक्स का उपयोग करती है।

## See Also (और जानकारी के लिए)
- [Swift Documentation](https://swift.org/documentation/)
- [Apple's Xcode Resources](https://developer.apple.com/xcode/)
- [SwiftUI Tutorial](https://developer.apple.com/tutorials/swiftui)
