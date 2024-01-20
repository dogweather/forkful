---
title:                "एक वेब पेज डाउनलोड करना"
html_title:           "Kotlin: एक वेब पेज डाउनलोड करना"
simple_title:         "एक वेब पेज डाउनलोड करना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
वेब पेज को डाउनलोड करना मतलब उसे अपने कंप्यूटर पर स्थानीय रूप से सहेजना। प्रोग्रामर्स इसे ऐसे स्थितियों में करते हैं जब उन्हें वेबसाइट की सामग्री का विश्लेषण करना होता है या वेब पेज की बैकअप करनी होती है।

## कैसे करें:
Fish Shell कोड ब्लॉक में कोडिंग उदाहरण और नमूना आउटपुट है।

```Fish Shell
# `curl` कमांड उपयोग करके वेब पेज डाउनलोड करें
curl -O http://example.com/index.html
```

यहां, `curl` कमांड http://example.com/index.html को डाउनलोड करेगा और इसे स्थानीय फ़ाइल `index.html` में सहेजेगा।

## गहरी डाइव:
"curl" का उपयोग वेब पेज को डाउनलोड करने में करने का इतिहास सन 1996 से है। वैकल्पिक उपकरण में "wget" और "HTTPie" शामिल हैं जो भी वेब पेज को डाउनलोड करने के लिए प्रयुक्त होते हैं। `curl` इसे ऐसे करता है कि यह HTTP GET अनुरोध भेजता है, जिसे वेब सर्वर द्वारा प्रतिक्रिया के रूप में HTML कोड भेजा जाता है।

## देखें भी:
1. curl मैन संस्करण - https://curl.se/docs/manpage.html
2. HTTPie गाइड - https://httpie.io/docs
3. "wget" का उपयोग कैसे करें - https://www.gnu.org/software/wget/manual/wget.html