---
title:                "वेब पृष्ठ डाउनलोड करना"
html_title:           "Bash: वेब पृष्ठ डाउनलोड करना"
simple_title:         "वेब पृष्ठ डाउनलोड करना"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्यों

ब्राउज़र में अपनी पसंदीदा वेबपेज को शामिल करने या जानकारी को ऑफलाइन उपलब्ध कराने के लिए आप एक वेबपेज डाउनलोड कर सकते हैं।

## कैसे करें

वेबपेज को डाउनलोड करने के लिए, आपको `curl` या `wget` कमांड का उपयोग करना होगा।

```Bash
# `curl` के द्वारा वेबपेज डाउनलोड करें
curl https://www.example.com > webpage.html

# `wget` के द्वारा वेबपेज डाउनलोड करें
wget https://www.example.com
```

अगर आपके पास `curl` या `wget` नहीं है, तो आप `apt-get` को उपयोग करके इनको इंस्टॉल कर सकते हैं। यदि आपको वेबपेज को ऐसे डाउनलोड करने की आवश्यकता है कि आप एक पासवर्ड दर्ज करें, तो आप `--user` और `--password` पैरामीटर का उपयोग कर सकते हैं।

```Bash
# प्रमाणीकरण सहित वेबपेज डाउनलोड करें
curl --user username:password https://www.example.com > webpage.html
```

अब आपके सिस्टम में वेबपेज सफलतापूर्वक डाउनलोड होगा और आप इसे अपनी पसंदीदा टेक्स्ट एडिटर में खोल सकते हैं।

## गहराई में जाएं

वेबपेज डाउनलोड करना वास्तव में निश्चित रूप से सरल है। आप `curl` और `wget` के अलावा अन्य उपकरणों का उपयोग करके भी कर सकते हैं। इन उपकरणों के साथ आप वेबपेज को समीक्षा और परिवर्तन के लिए भी आसानी से डाउनलोड कर सकते हैं।

## और भी देखें

[बश स्क्रिप्टिंग का आरंभ कैसे करें](https://www.digitalocean.com/community/tutorials/how-to-write-your-first-bash-script)

[Bash स्क्रिप्टिंग के प्रमुख अनुवाद](https://bash.cyber