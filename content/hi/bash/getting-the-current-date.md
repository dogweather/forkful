---
title:                "वर्तमान तिथि प्राप्त करना"
html_title:           "Bash: वर्तमान तिथि प्राप्त करना"
simple_title:         "वर्तमान तिथि प्राप्त करना"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्यों

आप जब अपने कम्प्यूटर में बाश कमांड लाइन यूज़ करते हैं तो आपको कभी-न-कभी अपने कम्प्यूटर पर वर्तमान दिनांक की जानकारी चाहिए होगी। शायद आप लॉग फ़ाइल्स को सिस्टम टाइमस्टैंप के साथ स्टैंप करके ट्रैक करना चाहते हों या नई फ़ाइलों को उनके बनाने की उपलब्धता देखना चाहते हों।

## कैसे करें

आप बाश में निम्नलिखित कमांड का उपयोग करके अपने कम्प्यूटर पर वर्तमान दिनांक को प्राप्त कर सकते हैं:

```Bash
date
```

यदि आप अपने कम्प्यूटर पर संचित दिनांक को बाहरी फ़ाइल में लिखना चाहते हैं, तो आप निम्नलिखित कमांड का उपयोग कर सकते हैं:

```Bash
date > date.txt
```

और यदि आप वर्तमान दिनांक के साथ टाइमस्टैंप भी सहित लिखना चाहते हों, तो निम्नलिखित कमांड का उपयोग कर सकते हैं:

```Bash
date +%F-%T > date.txt
```

यह कॉमांड आपको वर्तमान दिनांक को "YYYY-MM-DD-HH:MM:SS" के फॉर्मेट में लिखता है।

## गहराई में जाएं

विस्तृत जानकारी के लिए आप इस लिंक पर जा सकते हैं: [https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)

## देखें भी

[https://www.computerhope.com/unix/udate.htm](https://www.computerhope.com/unix/udate.htm)

[https://www.unixtutorial.org/bash-date-command/](https://www.unixtutorial.org/bash-date-command/)

[https://www.geeksforgeeks.org/date-command-linux-examples/](https://www.geeksforgeeks.org/date-command-linux-examples/)