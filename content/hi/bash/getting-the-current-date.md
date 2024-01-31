---
title:                "वर्तमान तारीख प्राप्त करना"
date:                  2024-01-20T15:13:33.777047-07:00
html_title:           "C: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"

category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
Bash में वर्तमान तारीख प्राप्त करना सिस्टम पर समय की जानकारी निकालने का तरीका है। प्रोग्रामर्स लॉग्स, रिपोर्ट और टाइम स्टैम्प्स बनाने के लिए इसका इस्तेमाल करते हैं।

## How to: (कैसे करें:)
```Bash
# वर्तमान दिनांक प्रदर्शित करें
date

# कस्टम फॉर्मेट में दिनांक और समय प्रदर्शित करें
date +"%Y-%m-%d %H:%M:%S"

# उदाहरण आउटपुट
2023-03-15 21:45:30
```

## Deep Dive (गहन जानकारी)
Bash में `date` कमांड UNIX के प्रारंभिक दिनों से उपयोग में है। यह C लाइब्रेरी के `strftime()` फंक्शन का लाभ उठाती है, जो विभिन्न फॉर्मेट विकल्पों के साथ समय दिखाने की क्षमता प्रदान करता है। `date` के विकल्पों में `+%` निर्दिष्टकर्ता का इस्तेमाल करके समय को अनुरूपित किया जा सकता है। वैकल्पिक तरीकों में GNU `date` कमांड, `printf` या `awk` शामिल हैं जो अधिक जटिल परिदृश्यों में उपयोगी होते हैं।

## See Also (सम्बन्धित जानकारी)
- GNU Coreutils `date` डॉक्युमेंटेशन: [https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- strftime फंक्शन फॉर्मेटिंग गाइड: [http://man7.org/linux/man-pages/man3/strftime.3.html](http://man7.org/linux/man-pages/man3/strftime.3.html)
