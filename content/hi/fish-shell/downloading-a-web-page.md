---
title:                "एक वेब पेज डाउनलोड करना"
html_title:           "Fish Shell: एक वेब पेज डाउनलोड करना"
simple_title:         "एक वेब पेज डाउनलोड करना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्यों

वेब पेज को डाउनलोड करने के लिए कोई क्यों लगाव दिखाएं ?
भारत में वर्तमान संस्करण फिश शैल (Fish Shell) सरलता, आसानी और शक्ति के साथ सिस्टम और वेब प्रोग्रामिंग का समर्थन करता है।

## कैसे

```
फिश शैल (Fish Shell) इंस्टॉल करें

sudo apt install fish
```

```
वेब पेज डाउनलोड करें

curl -O https://example.com/page.html
```

```
डाउनलोड की गई वेब पेज का नाम बदलें

mv page.html new_page.html
```

```
आपको पूरे वेब पेज के बजाय किसी खास हिस्से को ही डाउनलोड करना हो तो

curl -O https://example.com/page.html#section
```

```
एकाधिक वेब पेज को डाउनलोड करें

curl -O https://example.com/page1.html
curl -O https://example.com/page2.html
```

```
डाउनलोड की गई वेब पेज को दिखाएँ

cat page.html
```

## अधिक जानकारी

वेब पेज को डाउनलोड करने के लिए कई तरीके हैं। आपको कोई भी सुझाव या समस्या हो तो आप #fish चैनल पर IRC पर सहायता प्राप्त कर सकते हैं। मार्कडाउन (Markdown) संबंधित विस्तृत जानकारी यहां से प्राप्त कर सकते हैं - https://daringfireball.net/projects/markdown/ 

## देखें भी

- Fish Shell की आधिकारिक वेबसाइट - https://fishshell.com/
- Fish Shell के स्रोत कोड - https://github.com/fish-shell/fish-shell