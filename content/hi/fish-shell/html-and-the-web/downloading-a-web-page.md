---
date: 2024-01-20 17:44:53.834304-07:00
description: "\u0935\u0947\u092C \u092A\u0947\u091C \u0915\u094B \u0921\u093E\u0909\
  \u0928\u0932\u094B\u0921 \u0915\u0930\u0928\u093E \u092E\u0924\u0932\u092C \u0939\
  \u0948 \u0907\u0902\u091F\u0930\u0928\u0947\u091F \u0938\u0947 \u0909\u0938 \u092A\
  \u0947\u091C \u0915\u0940 \u0938\u093E\u092E\u0917\u094D\u0930\u0940 \u0915\u094B\
  \ \u0938\u0902\u0917\u094D\u0930\u0939\u093F\u0924 \u0915\u0930\u0928\u093E\u0964\
  \ \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0910\u0938\u093E\
  \ \u0921\u093E\u091F\u093E \u0935\u093F\u0936\u094D\u0932\u0947\u0937\u0923, \u092C\
  \u0948\u0915\u0905\u092A \u092F\u093E \u0911\u092B\u093C\u0932\u093E\u0907\u0928\
  \ \u092A\u0922\u093C\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0915\u0930\u0924\
  \u0947 \u0939\u0948\u0902\u0964"
lastmod: '2024-02-25T18:49:50.266048-07:00'
model: gpt-4-1106-preview
summary: "\u0935\u0947\u092C \u092A\u0947\u091C \u0915\u094B \u0921\u093E\u0909\u0928\
  \u0932\u094B\u0921 \u0915\u0930\u0928\u093E \u092E\u0924\u0932\u092C \u0939\u0948\
  \ \u0907\u0902\u091F\u0930\u0928\u0947\u091F \u0938\u0947 \u0909\u0938 \u092A\u0947\
  \u091C \u0915\u0940 \u0938\u093E\u092E\u0917\u094D\u0930\u0940 \u0915\u094B \u0938\
  \u0902\u0917\u094D\u0930\u0939\u093F\u0924 \u0915\u0930\u0928\u093E\u0964 \u092A\
  \u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0910\u0938\u093E \u0921\
  \u093E\u091F\u093E \u0935\u093F\u0936\u094D\u0932\u0947\u0937\u0923, \u092C\u0948\
  \u0915\u0905\u092A \u092F\u093E \u0911\u092B\u093C\u0932\u093E\u0907\u0928 \u092A\
  \u0922\u093C\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0915\u0930\u0924\u0947\
  \ \u0939\u0948\u0902\u0964"
title: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\
  \u0921 \u0915\u0930\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
वेब पेज को डाउनलोड करना मतलब है इंटरनेट से उस पेज की सामग्री को संग्रहित करना। प्रोग्रामर ऐसा डाटा विश्लेषण, बैकअप या ऑफ़लाइन पढ़ने के लिए करते हैं।

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
