---
title:                "कमांड लाइन आर्गुमेंट्स पढ़ना"
aliases: - /hi/bash/reading-command-line-arguments.md
date:                  2024-01-20T17:55:36.047117-07:00
model:                 gpt-4-1106-preview
simple_title:         "कमांड लाइन आर्गुमेंट्स पढ़ना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

कमांड लाइन आर्ग्यूमेंट्स पढ़ना यह है कि आप अपने बैश स्क्रिप्ट को चलाते समय इन्पुट कैसे पास करते हैं। प्रोग्रामर इसे इसलिए करते हैं क्योंकि इससे स्क्रिप्ट अधिक लचीली और पुन: उपयोगी बनती है।

## कैसे करें:

यहाँ कुछ कोड उदाहरण हैं:

```Bash
#!/bin/bash
echo "पहला आर्ग्यूमेंट: $1"
echo "दूसरा आर्ग्यूमेंट: $2"
echo "सारे आर्ग्यूमेंट: $@"
echo "आर्ग्यूमेंट्स की संख्या: $#"
```

यदि आप इसे `script.sh one two` के साथ चलाएं, आउटपुट होगा:

```
पहला आर्ग्यूमेंट: one
दूसरा आर्ग्यूमेंट: two
सारे आर्ग्यूमेंट: one two
आर्ग्यूमेंट्स की संख्या: 2
```

## गहराई से जानकारी:

कमांड लाइन आर्ग्यूमेंट्स UNIX और Linux शेल स्क्रिप्टिंग का मूलभूत हिस्सा हैं, जहाँ से बैश ने प्रेरणा ली है। इन्हें पढ़ने के लिए `$1`, `$2`, `$@`, और `$#` जैसे स्पेशल पैरामीटर्स का इस्तेमाल होता है। इसके विकल्प के तौर पर `getopts` आर्ग्यूमेंट पार्सर या फिर बड़ी स्क्रिप्ट्स के लिए `optarg` कमांड का उपयोग होता है। ये टूल आपको लॉन्ग ऑप्शंस और डिफ़ॉल्ट मानों के साथ-साथ वैलिडेशन भी प्रदान करते हैं।

## यह भी देखें:

- Bash man page (`man bash`): https://www.gnu.org/software/bash/manual/
- Advanced Bash-Scripting Guide: https://tldp.org/LDP/abs/html/
- Bash Hackers Wiki: https://wiki.bash-hackers.org/
- getopts tutorial: https://mywiki.wooledge.org/BashFAQ/035
