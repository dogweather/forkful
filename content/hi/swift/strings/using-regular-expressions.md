---
title:                "रेगुलर एक्सप्रेशन्स का उपयोग करना"
date:                  2024-02-03T19:19:24.215826-07:00
model:                 gpt-4-0125-preview
simple_title:         "रेगुलर एक्सप्रेशन्स का उपयोग करना"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
नियमित अभिव्यक्तियाँ, या regex, वे वर्णों के अनुक्रम होते हैं, जो एक खोज पैटर्न बनाते हैं, जिनका उपयोग अक्सर स्ट्रिंग मिलान या हेरफेर कार्यों के लिए किया जाता है। प्रोग्रामर डेटा मान्यता और पार्सिंग से लेकर रूपांतरणों तक, विभिन्न प्रोग्रामिंग भाषाओं में, स्विफ्ट सहित, पाठ संसाधन और हेरफेर कार्यों में इनका उपयोग करते हैं, जिससे ये एक अनिवार्य उपकरण बन जाते हैं।

## कैसे करें:
स्विफ्ट नेटिव समर्थन के लिए regex `NSRegularExpression` क्लास का उपयोग करता है, स्ट्रिंग क्लास के रेंज और प्रतिस्थापन विधियों के साथ। नीचे एक उदाहरण है, जो एक पाठ ब्लॉक के भीतर ईमेल पतों को खोजने और हाइलाइट करने के लिए regex का उपयोग करता है:

```swift
import Foundation

let text = "हमसे संपर्क करें support@example.com या feedback@example.org पर अधिक जानकारी के लिए।"
let regexPattern = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"

do {
    let regex = try NSRegularExpression(pattern: regexPattern)
    let matches = regex.matches(in: text, range: NSRange(text.startIndex..., in: text))

    if !matches.isEmpty {
        for match in matches {
            let range = Range(match.range, in: text)!
            print("पाया गया: \(text[range])")
        }
    } else {
        print("कोई मेल नहीं मिले।")
    }
} catch {
    print("Regex त्रुटि: \(error.localizedDescription)")
}

// नमूना आउटपुट:
// पाया गया: support@example.com
// पाया गया: feedback@example.org
```

अधिक जटिल या सुविधा-केंद्रित परिदृश्यों के लिए, आप स्विफ्टरेगेक्स जैसे तृतीय-पक्ष पुस्तकालयों का उपयोग कर सकते हैं, जो सिंटैक्स को सरल बनाता है और संभावनाओं को बढ़ाता है। हालांकि स्विफ्ट की मानक लाइब्रेरी शक्तिशाली है, कुछ डेवलपर्स उनके संक्षिप्त सिंटैक्स और अतिरिक्त सुविधाओं के लिए इन पुस्तकालयों को पसंद करते हैं। यहाँ कैसे आप एक समान कार्य को एक परिकल्पित तृतीय-पक्ष पुस्तकालय का उपयोग कर कर सकते हैं:

```swift
// मान लीजिए कि एक पुस्तकालय को स्विफ्टरेगेक्स कहा जाता है और यह आयातित है
let text = "हमसे संपर्क करें hello@world.com पर या हमारी वेबसाइट पर जाएँ।"
let emailPattern = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"

let emails = text.matches(for: emailPattern) // स्विफ्टरेगेक्स द्वारा प्रदानित परिकल्पना विधि
if emails.isEmpty {
    print("कोई ईमेल पते नहीं मिले।")
} else {
    emails.forEach { email in
        print("पाया गया: \(email)")
    }
}

// परिकल्पित आउटपुट मानते हुए `matches(for:)` विधि स्विफ्टरेगेक्स में मौजूद है:
// पाया गया: hello@world.com
```

यह उदाहरण एक स्ट्रिंग के भीतर मेल खोजने को सरल बनाने के लिए एक तृतीय-पक्ष नियमित अभिव्यक्ति पैकेज का उपयोग करता है, मान कर कि `matches(for:)` जैसे सुविधा विधियाँ मौजूद हैं। सही सिंटैक्स और विधि उपलब्धता के लिए संबंधित तृतीय-पक्ष पुस्तकालय दस्तावेज़ का संदर्भ लेना महत्वपूर्ण है।
