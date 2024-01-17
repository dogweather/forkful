---
title:                "स्ट्रिंग्स को सम्मिलित करना"
html_title:           "Swift: स्ट्रिंग्स को सम्मिलित करना"
simple_title:         "स्ट्रिंग्स को सम्मिलित करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

# Concatenating Strings in Swift: What and Why?

Concatenating strings in Swift is the process of combining two or more strings into a single string. Programmers do this in various scenarios, such as creating dynamic text messages, combining user input with predefined text, or constructing complex sentences.

# कैसे करें और क्यों करें?

एस-स्विफ्ट में स्ट्रिंग्स को एक साथ जोड़ने की प्रक्रिया को कंकटेनेशन करना कहा जाता है। प्रोग्रामर इसे विभिन्न परिस्थितियों में करते हैं, जैसे कि डायनामिक टेक्स्ट मैसेज बनाना, उपयोगकर्ता इनपुट को पूर्वनिर्धारित पाठ के साथ जोड़ना, या जटिल वाक्य बनाना।

## कैसे करें?

```Swift
var greeting = "नमस्ते"
var name = "आप"
var sentence = greeting + " " + name + ", कैसे हो?"

print(sentence)
// नमस्ते आप, कैसे हो?
```

यहां, हमने तीन अलग-अलग स्ट्रिंग्स को जोड़कर एक सिंगल स्ट्रिंग बनाई है और फिर उसे प्रिंट किया है।

## गहराई में जाएं

ककटेनेशन की विशाल विश्लेषण के साथ, हम अभिकल्पों के अन्य विकल्पों और इसके संचालन देखेंगे।

### ऐतिहासिक पृष्ठभूमि

पहले से ही, स्ट्रिंग्स को एक साथ जोड़ने का प्रक्रियाई समझने के लिए, स्ट्रिंग्स को बाइट-स्तर से एक साथ जोड़ा जाता था, तो इसे अंतरिक्ष के लिए एक करोड़ अलग स्ट्रिंग पूल के साथ होना पड़ता था। स्विफ्ट में, इस प्रक्रिया को सरल बनाने के लिए, एक अंतरिक्ष नियंत्रक है जो ग्राहक आवश्यक कम अपने अनुकूल विकास संस्करण नहीं देता। इसका परिणाम है कि स्विफ्ट ककटनेकेनिंग स्पेस हमेशा धन्यवाद होता है।

### वैकल्पिक

ककटेनेशन को बड़े संचारों की ऋण और सामान्य प्रक्रिया सामग्री में भगोड़ा होने के लिए देखा जाता है। इन परिस्थियों में, आमतौर पर स्विफ्ट में पसंदीदा स्ट्रिंग फॉर्मेटिंग विधि स्ट्रिंग बिल्ट-इन asoni. स्कलोन और देता है।

## इसके अलावा

स्ट्रिंग्स को एक साथ जोड़ने के संबंध में अन्य संसाधनों के लिए इस साइट पर जाएं  [Swift आधिकारिक रीफरेंस संज्ञान।](https://developer.apple.com/documentation/swift/string/)

अगर आपको ऑडियो स्पेलिंग काम करने के लिए घंटे चाहिए, तो आपको यह धन्यवाद है ike recognize और हेराम game भी मदद।