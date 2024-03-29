---
date: 2024-01-20 18:04:47.695103-07:00
description: "\u0928\u0908 \u092A\u094D\u0930\u094B\u091C\u0947\u0915\u094D\u091F\
  \ \u0936\u0941\u0930\u0942 \u0915\u0930\u0928\u093E \u092A\u094D\u0930\u094B\u0917\
  \u094D\u0930\u093E\u092E\u093F\u0902\u0917 \u092E\u0947\u0902 \u090F\u0915 \u0924\
  \u093E\u091C\u093E \u092A\u0924\u094D\u0924\u093E \u092A\u0932\u091F\u0928\u093E\
  \ \u0939\u094B\u0924\u093E \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\
  \u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0947 \u0939\u0930 \u092C\u093E\
  \u0930 \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u091C\u092C \u0909\u0928\u094D\
  \u0939\u0947\u0902 \u0928\u0908 \u0909\u092A\u0932\u092C\u094D\u0927\u093F\u092F\
  \u094B\u0902 \u0915\u0940 \u0924\u0932\u093E\u0936 \u0939\u094B\u0924\u0940 \u0939\
  \u0948 \u092F\u093E \u0915\u094B\u0908 \u0928\u092F\u093E\u2026"
lastmod: '2024-03-13T22:44:53.067542-06:00'
model: gpt-4-1106-preview
summary: "\u0928\u0908 \u092A\u094D\u0930\u094B\u091C\u0947\u0915\u094D\u091F \u0936\
  \u0941\u0930\u0942 \u0915\u0930\u0928\u093E \u092A\u094D\u0930\u094B\u0917\u094D\
  \u0930\u093E\u092E\u093F\u0902\u0917 \u092E\u0947\u0902 \u090F\u0915 \u0924\u093E\
  \u091C\u093E \u092A\u0924\u094D\u0924\u093E \u092A\u0932\u091F\u0928\u093E \u0939\
  \u094B\u0924\u093E \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\
  \u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0947 \u0939\u0930 \u092C\u093E\u0930\
  \ \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u091C\u092C \u0909\u0928\u094D\u0939\
  \u0947\u0902 \u0928\u0908 \u0909\u092A\u0932\u092C\u094D\u0927\u093F\u092F\u094B\
  \u0902 \u0915\u0940 \u0924\u0932\u093E\u0936 \u0939\u094B\u0924\u0940 \u0939\u0948\
  \ \u092F\u093E \u0915\u094B\u0908 \u0928\u092F\u093E\u2026"
title: "\u0928\u0908 \u092A\u0930\u093F\u092F\u094B\u091C\u0928\u093E \u0936\u0941\
  \u0930\u0942 \u0915\u0930\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
नई प्रोजेक्ट शुरू करना प्रोग्रामिंग में एक ताजा पत्ता पलटना होता है। प्रोग्रामर्स इसे हर बार करते हैं जब उन्हें नई उपलब्धियों की तलाश होती है या कोई नया विचार आता है जो उन्होंने कोड के रूप में परिणत करना चाहते हैं।

## How to: (कैसे करें:)
Fish Shell में प्रोजेक्ट शुरू करने के लिए आपको मूल डायरेक्टरी सेटअप और शुरुआती फाइलें बनानी होती हैं। नीचे आपको एक उदाहरण मिलेगा।

```Fish Shell
# पहले एक नया डायरेक्टरी बनाएं
mkdir my_new_project

# डायरेक्टरी में जाएं
cd my_new_project

# एक बुनियादी स्क्रिप्ट फाइल बनाएं
echo "#!/usr/bin/env fish" > start.fish
chmod +x start.fish

# डायरेक्टरी स्ट्रक्चर चेक करें
tree .
```

सैंपल आउटपुट:
```
.
├── start.fish
```

## Deep Dive (गहराई से जानकारी)
Fish Shell, अपने आधुनिक सिंटैक्स और उपयोगिता के लिए प्रसिद्ध है। यह बैश और जेडएसएच का एक अल्टरनेटिव है और इसे सबसे पहले 2005 में रिलीज किया गया था। Fish में स्क्रिप्टिंग की अपनी खूबियां हैं जैसे कि ऑटो-सजेशन्स, सिंटैक्स हाइलाइटिंग और एक फ्रेंडली और विस्तृत यूजर डॉक्यूमेंटेशन। जब आप नया प्रोजेक्ट शुरू करते हैं, तो आप शुरुआती फाइल्स और डायरेक्टरीज को सेटअप करके, साथ ही एक प्रोजेक्ट की लाइफसाइकिल को मैनेज करने के लिए विभिन्न स्क्रिप्ट्स और टूल्स का उपयोग करके संरचना बना सकते हैं।

## See Also (देखें भी)
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html) - Fish का ऑफिसियल डॉक्यूमेंटेशन।
- [Fish Scripting Tutorial](https://fishshell.com/docs/current/tutorial.html) - Fish में स्क्रिप्ट लिखने के लिए एक ट्यूटोरिअल।
- [Awesome Fish](https://github.com/jorgebucaran/awesome-fish) - Fish रिलेटेड रिसोर्सेज़ और प्लगइंस का कलेक्शन।
