---
title:                "एक पैटर्न से मेल खाते अक्षरों को हटाना"
html_title:           "Fish Shell: एक पैटर्न से मेल खाते अक्षरों को हटाना"
simple_title:         "एक पैटर्न से मेल खाते अक्षरों को हटाना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
अगर आप एक प्रोग्रामर हैं, तो आपने कभी न कभी किसी पैटर्न के समान चरित्रों को हटाने के बारे में सुना होगा। इसका मतलब है कि आप एक पैटर्न द्वारा निर्दिष्ट चरित्रों को संसाधित करके उन्हें हटाना चाहिए। प्रोग्रामर इसका यूज़ क्यों करते हैं? यह आमतौर पर text processing और data manipulation के लिए इस्तेमाल किया जाता है।

## कैसे करें:
तो अब हम आपको बताएंगे कि आप फिश शैल में पैटर्न के समान चरित्रों को कैसे हटा सकते हैं।

```
Fish Shell में चरित्रों को हटाने का उदाहरण:

# जब सारे बड़े अक्षरों को हटाएँ
$ tr -cd '[:lower:]'

इनपुट:
Hello World!

आउटपुट:
ello orld

```

## गहराई में जाएं:
अधिक गहराई में जाने से पहले, आपको पता होना चाहिए कि यह कोडिंग टेक्निक कैसे develop हुई। इसका पहला रूप 'tr' command ने Bell Labs में 1969 में अपने original UNIX में किया था। लेकिन आजकल यह feature हमें बहुत सारे different तरीकों से मिल रहा है, जिनमें Perl Compatible Regular Expressions (PCRE) एवं Glob patterns शामिल हैं। Fish Shell का उसे करने का तरीका Perl पर based है।

## इससे संबंधित:
अगर आपको फिश शैल के बारे में और अधिक जानना है तो आप ये लिंक्स check कर सकते हैं:

- [Fish Shell वेबसाइट](https://fishshell.com/)
- [Fish Shell की डॉक्यूमेंटेशन](https://fishshell.com/docs/current/index.html)
- [फिश शैल के अधिकरों के लेख](https://github.com/fish-shell/fish-shell/wiki/More-reads)