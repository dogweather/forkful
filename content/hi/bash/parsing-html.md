---
title:                "HTML पार्स करना"
html_title:           "C++: HTML पार्स करना"
simple_title:         "HTML पार्स करना"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/parsing-html.md"
---

{{< edit_this_page >}}

## क्या & क्यों?

HTML पार्सिंग, एक HTML डॉक्युमेंट को विश्लेषित करने और उसके भितर की जानकारी का उपयोग करने की प्रक्रिया है। प्रोग्रामर्स इसे डाटा खोजने, डाटा माइनिंग, और वेब स्क्रेपिंग के लिए करते हैं। 

## कैसे करें:

आप bash में, `grep` और `sed` के साथ HTML पार्स कर सकते हैं:

```Bash
curl -s https://www.example.com | grep -o '<title>[^<]*</title>' | sed 's/<title>\(.*\)<\/title>/\1/'
```

इस उदाहरण में, `curl` वेब पेज को डाउनलोड करता है, `grep` `<title>` टैग को खोजता है, और `sed` टाइटल की मूल जानकारी को बाहर निकालता है। 

## गहराई में: 

HTML पार्सिंग की जरूरत 1990 के दशक के बाद उभरी, जब वेब उभरा और जानकारी अधिक से अधिक डिजिटल बनी। बाश से पहले, Perl, Python जैसी भाषाओं का उपयोग अधिक था। बाश का उपयोग करते समय, आपको मिल सकती हैं त्रुटियों से, जैसे कि गलत HTML, जो एक उच्च स्तरीय भाषा जैसे python द्वारा हेंडल हो सकती है। 

## देखने के लिए: 

अधिक विस्तार से जानने के लिए, निम्नलिखित स्रोतों पर जाएं:

2. [HTML Parsing with Python](https://realpython.com/beautiful-soup-web-scraper-python/)