---
title:                "वेब पेज डाउनलोड करना"
date:                  2024-01-20T17:43:39.881581-07:00
model:                 gpt-4-1106-preview
simple_title:         "वेब पेज डाउनलोड करना"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
वेब पेज डाउनलोड करना इसकी HTML सामग्री को स्थानीय सिस्टम पर सहेजना होता है। प्रोग्रामर्स इसे डेटा स्क्रैपिंग, ऑफलाइन पढ़ने या वेब आर्काइविंग के लिए करते हैं।

## How to (कैसे करें):
```Bash
# cURL का इस्तेमाल करके वेब पेज डाउनलोड करना
curl https://example.com -o example.html

# wget का उपयोग करके पूरी वेबसाइट डाउनलोड करना
wget -r -p -U Mozilla https://example.com

# यहाँ कुछ सैंपल आउटपुट है:
# सफल cURL कमांड
% Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                Dload  Upload   Total   Spent    Left  Speed
100  1270  100  1270    0     0   6350      0 --:--:-- --:--:-- --:--:--  6350

# सफल wget कमांड
‘example.com/index.html’ saved [1270/1270]
```

## Deep Dive (गहराई में जानकारी):
पहले, लोग मुख्यतः FTP का उपयोग करके फाइलें डाउनलोड करते थे। वेब आगमन के बाद, HTTP और HTTPS प्रोटोकॉल्स ने तेज़ी से लोकप्रियता प्राप्त की। cURL और wget दो शक्तिशाली टूल्स हैं जो HTTP, HTTPS सहित कई प्रोटोकॉल्स का समर्थन करते हैं। cURL लाइब्रेरी libcurl का उपयोग करता है, जबकि wget एक स्वतंत्र यूटिलिटी है जो रिकर्सिव डाउनलोडिंग के लिए बेहतर है। वेब पेज डाउनलोड करते वक्त, ऐसे भी कई विकल्प हैं जैसे कि यूजर-एजेंट को स्पेसिफाई करना या केवल निश्चित फाइल टाइप्स को डाउनलोड करना।

## See Also (और भी देखें):
- cURL मैनुअल: https://curl.se/docs/manpage.html
- Wget मैनुअल: https://www.gnu.org/software/wget/manual/wget.html
- HTTP/HTTPS प्रोटोकॉल्स पर विस्तार में जानकारी: https://developer.mozilla.org/en-US/docs/Web/HTTP
- लिबकर्ल (libcurl) डेवलपर्स गाइड: https://curl.se/libcurl/c/libcurl.html
- विकिपीडिया पर FTP: https://hi.wikipedia.org/wiki/फाइल_स्थानांतरण_प्रोटोकॉल
