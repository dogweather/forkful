---
date: 2024-01-20 17:54:46.941901-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) ."
lastmod: '2024-03-13T22:44:53.096623-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093C\u093E\u0907\u0932\
  \ \u092A\u0922\u093C\u0928\u093E"
weight: 22
---

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
