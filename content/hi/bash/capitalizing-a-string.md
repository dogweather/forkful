---
title:                "एक स्ट्रिंग को बड़े अक्षरों में बदलना"
html_title:           "Bash: एक स्ट्रिंग को बड़े अक्षरों में बदलना"
simple_title:         "एक स्ट्रिंग को बड़े अक्षरों में बदलना"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

वर्णमाला के निर्देशन (यानी कोई string capitalizing) का मतलब है कि string के अक्षरों को बड़ा (upper-case) करना। प्रोग्रामर इसे readability सुधारने और डाटा को consistent रखने के लिए करते हैं।

## कैसे करें:

Bash में आप `tr` command का उपयोग करके string को capitalize कर सकते हैं। यह command एक से अधिक character को replace करती है।

```Bash
echo 'hello world' | tr '[:lower:]' '[:upper:]'
```

इसे चलाने से आपको 'HELLO WORLD' का output मिलेगा।

## गहराई की जांच:

कई पुराने Unix और Linux systems में यह `tr` command पहले से मौजूद थी, इसलिए इसे बहुत commonly use किया गया। लेकिन, Bash (4.0 और इसके ऊपर) का upcase function इस task के लिए और अधिक suitable है। यह इस प्रकार होता है:

```Bash
string="hello world"
echo "${string^^}"
```

इस के चलने पर आपको 'HELLO WORLD' जैसा output मिलेगा।

## और देखें:

1. [Bash scripting guide](https://devhints.io/bash)
3. [FreeCodeCamp article on Bash commands](https://www.freecodecamp.org/news/the-linux-commands-handbook/)