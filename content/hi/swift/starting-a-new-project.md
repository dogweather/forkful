---
title:                "नया प्रोजेक्ट शुरू करना"
html_title:           "C: नया प्रोजेक्ट शुरू करना"
simple_title:         "नया प्रोजेक्ट शुरू करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/starting-a-new-project.md"
---

{{< edit_this_page >}}

# एक नई Swift प्रोजेक्ट शुरू करें

## क्या और क्यों?
एक नई प्रोजेक्ट शुरू करना का मतलब होता है किसी नए सॉफ्टवेयर या ऐप्लिकेशन का विकास शुरू करना। प्रोग्रामर इसे तभी करते हैं जब वह उनके पास कोई नया विचार होता है या उन्हें एक ऐसे समस्या का समाधान करना होता है जिसका समाधान पहले से उपलब्ध नहीं होता है।

## कैसे करें:
आप निम्नलिखित Swift कोड का उपयोग करके एक नई प्रोजेक्ट शुरू कर सकते हैं:

```Swift
import SwiftUI

struct ContentView: View {
    var body: some View {
        Text("नमस्ते, दुनिया!")
            .padding()
    }
}

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
```

ऊपरी कोड स्निपेट एक नई SwiftUI प्रोजेक्ट की बुनियाद बना देगा, "नमस्ते, दुनिया!" लिखने वाला एक साधारण टैक्स्ट व्यू बनाएंगे।

## गहराई में जाना:
नई प्रोजेक्ट की शुरुआत करने के लिए, Swift ने अपने लेटेस्ट अपडेट के रूप में SwiftUI का उपयोग किया है, जो एक डेक्लेरेटिव UI फ्रेमवर्क है। 

ऐल्टरनेटिवली, आप UIKit जैसे पुराने फ्रेमवर्क का उपयोग भी कर सकते हैं, लेकिन SwiftUI की तुलना में इसका सिंटैक्स ज़्यादा विस्तृत है।

लागतम क्रमबद्धता के लिए, नई प्रोजेक्ट शुरू करते समय MVVM (Model-View-ViewModel) या MVC (Model-View-Controller) जैसे डिज़ाइन पैटर्न का उपयोग करना सर्वश्रेष्ठ है।

## देखें भी:
1. [SwiftUI ट्यूटोरियल](https://developer.apple.com/tutorials/swiftui)
2. [MVVM और MVC के बारे में जानकारी](https://www.raywenderlich.com/6733535-ios-mvvm-tutorial-refactoring-from-mvc)
3. [UIKit vs SwiftUI](https://www.raywenderlich.com/5429279-swiftui-vs-uikit-which-should-you-use)