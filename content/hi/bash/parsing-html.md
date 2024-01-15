---
title:                "Html को विश्लेषण करना"
html_title:           "Bash: Html को विश्लेषण करना"
simple_title:         "Html को विश्लेषण करना"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/parsing-html.md"
---

{{< edit_this_page >}}

## क्यों
आपने शायद कभी कोई वेब पृष्ठ (HTML) देखा होगा और बेहद चाहते होंगे कि उस पेज की संरचना और आकार क्या है। ऐसे में, आपको HTML को parse करने की जरूरत पड़ सकती है।

## कैसे करें
यदि आपको किसी वेब पृष्ठ को parse करने की जरूरत है तो आप कुछ आसान बातों का ध्यान रख सकते हैं। इन बातों को follow करके आप किसी भी पेज को आसानी से पार्स कर सकते हैं।

```Bash
# आपको पहले किसी HTML पेज का URL ढूंढना होगा। इसके बाद नीचे दिए गए एक्सैंपल कोड का उपयोग करके उस पेज का source code ले सकते हैं

$ curl https://www.example.com > webpage.html

# अब HTML पेज का सारा source code webpage.html नाम के file में save हो जायेगा। अब आप इस file को किसी text editor में open करके पूरा code देख सकते हैं

```

## डीप डाइव
HTML पेज को parse करना थोड़ा जटिल हो सकता है। आपको HTML या CSS के बारे में थोड़ी सी जानकारी होनी चाहिए ताकि आप सटीक तरीके से पेज को parse कर सकें। आपको इसमें अलग-अलग tags, attributes और values की समझ होनी चाहिए।

## देखें भी
अगर आप और भी अधिक जानकरी चाहते हैं तो नीचे दिए गए लिंक्स को देख सकते हैं:

- [वेब डेवलपमेंट के बेसिक्स](https://www.geeksforgeeks.org/web-development-basics/)
- [HTML, CSS और JavaScript की जानकारी](https://developer.mozilla.org/en-US/docs/Learn/Getting_started_with_the_web)
- [ऑनलाइन HTML को parser करने के लिए टूल्स](https://html-parser.com/)

**Happy parsing!**