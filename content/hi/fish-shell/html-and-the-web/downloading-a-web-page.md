---
title:                "वेब पेज डाउनलोड करना"
aliases:
- /hi/fish-shell/downloading-a-web-page/
date:                  2024-01-20T17:44:53.834304-07:00
model:                 gpt-4-1106-preview
simple_title:         "वेब पेज डाउनलोड करना"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/downloading-a-web-page.md"
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
