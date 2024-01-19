---
title:                "वर्तमान तारीख प्राप्त करना"
html_title:           "C#: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्या और क्यों? 
वर्तमान तारीख प्राप्त करने का मतलब होता है की कल्पनात्मक मैशिने सिस्टम की वर्तमान तिथि और समय पढ़ना। यह प्राप्त करने के लिए कोडिंग करते हैं क्योंकि यह अक्सर तारीख और समय-संभंधित प्रोग्राम्स के लिए महत्वपूर्ण होता है, जैसे कि लॉग फ़ाइल बनाने या किसी इवेंट को ट्रैक करने के लिए।

## कैसे करें: 
आप Bash में `date` कमांड का उपयोग करके सिस्टम की वर्तमान तारीख और समय प्राप्त कर सकते हैं:

```Bash
date
```

यह कमांड वर्तमान तारीख और समय को निम्नलिखित फ़ॉर्मेट में प्रिंट करेगा:

```Bash
Wed Apr 21 17:15:17 PDT 2021
```

## गहराई की जांच: 
तारीख प्राप्त करने का यह तरीका Bash के पुराने वर्जनों से आता है। एल्टरनेटिव्स में `printf` बी शामिल है:

```Bash
printf "%(%Y-%m-%d)T\n" -1 
```

यह कमांड वर्तमान तारीख को 'YYYY-MM-DD' फ़ॉर्मेट में प्रिंट करेगा। `-1` का उपयोग वर्तमान समय और तारीख के लिए किया जाता है।

## देखने के लिए: 
1. [गीतहब: Bash Date Command](https://github.com/Idnan/bash-guide#date-command)
2. [Stack Overflow: Get the Current Date and Time](https://stackoverflow.com/questions/1401482/yyyy-mm-dd-format-date-in-shell-script)