---
title:                "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
html_title:           "Kotlin: एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
simple_title:         "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
मानवीं, एक स्ट्रिंग को लोअर केस में बदलना मतलब उसके सभी अक्षरों को छोटे अक्षरों में बदलना है। प्रोग्रामर्स यह इसलिए करते हैं ताकि उन्हें डाटा के केस संवेदनशीलता से चिंता न करनी पड़े।

## कैसे करें:
`Bash` में, आप `tr` कमांड का उपयोग करके एक स्ट्रिंग को लोअरकेस में बदल सकते हैं:

```Bash
string="HELLO WORLD"
lowercase_string=$(echo "$string" | tr '[:upper:]' '[:lower:]')
echo $lowercase_string
```

जो उत्पादन होगा:

```Bash
hello world
```

## गहरा डाइव:
ऐतिहासिक सन्दर्भ में, `Bash` ने `tr` कमांड के साथ-साथ `awk` और `sed` जैसे उपकरणों का भी समर्थन किया है जिससे स्ट्रिंग्स को लोअरकेस में बदला जा सकता है। हालांकि, `tr` कमांड सबसे सीधा और आसान तरीका है।

## देखिए भी:
1. [GNU Bash डॉक्यूमेंटेशन](https://www.gnu.org/software/bash/)
2. [tr कमांड का उपयोग करना](http://www.linfo.org/tr.html)
3. [Bash में स्ट्रिंग्स को लोअरकेस में बदलने के अन्य तरीके](https://www.cyberciti.biz/faq/unix-linux-idowncase-string-bash-ksh-shell/)