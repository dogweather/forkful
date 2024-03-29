---
date: 2024-01-20 17:54:46.941901-07:00
description: "\u092A\u093E\u0920 \u092B\u093C\u093E\u0907\u0932 (text file) \u092A\
  \u0922\u093C\u0928\u093E \u0907\u0938\u0915\u0940 \u0938\u093E\u092E\u0917\u094D\
  \u0930\u0940 \u0915\u094B \u0932\u094B\u0921 \u0914\u0930 \u092A\u094D\u0930\u0926\
  \u0930\u094D\u0936\u093F\u0924 \u0915\u0930\u0928\u0947 \u0915\u0940 \u092A\u094D\
  \u0930\u0915\u094D\u0930\u093F\u092F\u093E \u0939\u0948\u0964 \u092A\u094D\u0930\
  \u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u092F\u0947 \u0915\u093E\
  \u092E \u0921\u0947\u091F\u093E \u0938\u0902\u0938\u093E\u0927\u0928, \u092A\u0930\
  \u0940\u0915\u094D\u0937\u0923, \u0914\u0930 \u0938\u094D\u0915\u094D\u0930\u093F\
  \u092A\u094D\u091F \u0915\u0949\u0928\u094D\u092B\u093C\u093F\u0917\u094D\u092F\u0942\
  \u0930\u0947\u0936\u0928\u2026"
lastmod: '2024-03-13T22:44:53.096623-06:00'
model: gpt-4-1106-preview
summary: "\u092A\u093E\u0920 \u092B\u093C\u093E\u0907\u0932 (text file) \u092A\u0922\
  \u093C\u0928\u093E \u0907\u0938\u0915\u0940 \u0938\u093E\u092E\u0917\u094D\u0930\
  \u0940 \u0915\u094B \u0932\u094B\u0921 \u0914\u0930 \u092A\u094D\u0930\u0926\u0930\
  \u094D\u0936\u093F\u0924 \u0915\u0930\u0928\u0947 \u0915\u0940 \u092A\u094D\u0930\
  \u0915\u094D\u0930\u093F\u092F\u093E \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\
  \u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u092F\u0947 \u0915\u093E\u092E\
  \ \u0921\u0947\u091F\u093E \u0938\u0902\u0938\u093E\u0927\u0928, \u092A\u0930\u0940\
  \u0915\u094D\u0937\u0923, \u0914\u0930 \u0938\u094D\u0915\u094D\u0930\u093F\u092A\
  \u094D\u091F \u0915\u0949\u0928\u094D\u092B\u093C\u093F\u0917\u094D\u092F\u0942\u0930\
  \u0947\u0936\u0928\u2026"
title: "\u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093C\u093E\u0907\u0932\
  \ \u092A\u0922\u093C\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
पाठ फ़ाइल (text file) पढ़ना इसकी सामग्री को लोड और प्रदर्शित करने की प्रक्रिया है। प्रोग्रामर्स ये काम डेटा संसाधन, परीक्षण, और स्क्रिप्ट कॉन्फ़िग्यूरेशन हेतु करते हैं।

## How to: (कैसे करें:)
```Fish Shell
# एक फ़ाइल से सीधे पढ़ने के लिए
cat filename.txt

# पाठ को पंक्तिवार पढ़ने के लिए
while read -la line
    echo $line
end < filename.txt
```
सैंपल आउटपुट:
```
यह फाइल की पहली पंक्ति है।
यह दूसरी पंक्ति है।
...
```

## Deep Dive (गहराई से जानकारी):
पाठ फ़ाइल पढ़ने की क्षमता पुरानी और बुनियादी है, UNIX जैसे प्रणालियों से उत्पन्न होती है। फ़ाइल सिस्टम के साथ इंटरैक्ट करना एक मौलिक कार्य है। Fish Shell उपयोग में सरल है और पारंपरिक Bash की तुलना में कुछ संवर्दित फीचर्स प्रदान करता है। उदाहरण के लिए, `read` कमांड ऑटोमैटिकली स्पिल्टिंग और लूपिंग को संभालता है। 

वैकल्पिक रूप से, `awk`, `sed`, `grep`, जैसे उपकरण भी पाठ प्रसंस्करण के लिए प्रयोग किए जाते हैं, परंतु Fish में सामान्यत: सीधी कमांड्स का उपयोग होता है। अतिरिक्त, Fish बेहतर सहायता और यूआई फ्रेंडली सुविधाएँ जैसे सिंटैक्स हाइलाइटिंग और ऑटो-सजेशन देता है।

## See Also (संबंधित सूत्र):
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Learn X in Y minutes: Fish](https://learnxinyminutes.com/docs/fish/)
- [Fish Shell GitHub repository](https://github.com/fish-shell/fish-shell)
