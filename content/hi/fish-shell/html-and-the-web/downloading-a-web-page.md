---
date: 2024-01-20 17:44:53.834304-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Fish Shell\
  \ \u092E\u0947\u0902 \u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\
  \u0932\u094B\u0921 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F `curl`\
  \ \u0915\u092E\u093E\u0902\u0921 \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\
  \u093E\u0932 \u0915\u0930\u0947\u0902."
lastmod: '2024-03-13T22:44:53.064218-06:00'
model: gpt-4-1106-preview
summary: "Fish Shell \u092E\u0947\u0902 \u0935\u0947\u092C \u092A\u0947\u091C \u0921\
  \u093E\u0909\u0928\u0932\u094B\u0921 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\
  \u093F\u090F `curl` \u0915\u092E\u093E\u0902\u0921 \u0915\u093E \u0907\u0938\u094D\
  \u0924\u0947\u092E\u093E\u0932 \u0915\u0930\u0947\u0902."
title: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\
  \u0921 \u0915\u0930\u0928\u093E"
weight: 42
---

## How to: (कैसे करें:)
Fish Shell में वेब पेज डाउनलोड करने के लिए `curl` कमांड का इस्तेमाल करें:

```Fish Shell
# वेब पेज डाउनलोड करना
curl http://example.com -o saved_page.html

# डाउनलोड की गई फाइल की सामग्री देखना
cat saved_page.html
```

उपरोक्त कमांड से सरवर से page की HTML फ़ाइल saved_page.html के नाम से आपके सिस्टम पर सेव हो जाएगी। आपको कुछ इस तरह का आउटपुट मिलेगा:

```html
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</head>

<body>
<div>
    ...
</div>
</body>
</html>
```

## Deep Dive (गहराई में जानकारी)
वेब पेज डाउनलोडिंग का काम `wget` और `curl` जैसे टूल्स से हो सकता है। `wget` पूरी वेबसाइट्स डाउनलोड करने में मजबूत माना जाता है, वहीं `curl` एक्सटेंसिबिलिटी में बेहतर है। Fish Shell, जो फ्रेंडली और यूजर सेंट्रिक है, ने भी इन टूल्स को सपोर्ट किया है जिससे Automation और Scripting में आसानी हो। HTTP/HTTPS, FTP, और अन्य प्रोटोकॉल्स का इस्तेमाल करते हुए, आप फ़ाइलें और डेटा जल्दी से डाउनलोड कर सकते हैं।

## See Also (और भी जानकारी)
- Fish Shell Documentation: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- cURL Documentation: [https://curl.se/docs/](https://curl.se/docs/)
- Wget Documentation: [https://www.gnu.org/software/wget/manual/wget.html](https://www.gnu.org/software/wget/manual/wget.html)
