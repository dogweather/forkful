---
title:                "नई परियोजना शुरू करना"
date:                  2024-01-20T18:05:05.077244-07:00
model:                 gpt-4-1106-preview
simple_title:         "नई परियोजना शुरू करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
Swift में नया प्रोजेक्ट शुरू करना एक खाली कैनवास पे पेंटिंग शुरू करने जैसा है — सब कुछ बनाने की शुरुआत। प्रोग्रामर्स नए प्रोजेक्ट इसलिए शुरू करते हैं क्योंकि नए आइडिया को कोड की दुनिया में लाने की जरूरत होती है या मौजूदा समस्याओं के हल ढूँढने होते हैं।

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