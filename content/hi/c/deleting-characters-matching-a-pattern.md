---
title:                "पैटर्न से मिलते जुलते वर्णों को हटाना"
html_title:           "Elixir: पैटर्न से मिलते जुलते वर्णों को हटाना"
simple_title:         "पैटर्न से मिलते जुलते वर्णों को हटाना"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

निर्दिष्ट पैटर्न के अनुसार वर्णों को हटाने का अर्थ होता है किसी स्ट्रिंग से विशिष्ट वर्णों को हटाना। प्रोग्रामर्स इसे तब करते हैं जब उन्हें स्ट्रिंग को कुछ विशिष्ट रूप में दिखाने की आवश्यकता होती है।

## कैसे करें:

ये कोडिंग उदाहरण स्ट्रिंग से विशिष्ट वर्णों को हटाना दिखाते हैं:

```C
#include<stdio.h>
#include<string.h>

void removeChars(char *str, char *remove) {
  int i = 0, j = 0;
  char temp[100];
  
  while(str[i]) {
    if(strchr(remove, str[i]) == NULL) {
      temp[j] = str[i];
      j++;
    }
    i++;
  }
  temp[j] = '\0';
  
  strcpy(str, temp);
}

int main() {
  char str[] = "Hello, world!";
  char remove[] = "l";
  
  removeChars(str, remove);
  
  printf("%s\n", str);
  
  return 0;
}
```

और आउटपुट होगा:

```
Heo, word!
```

## गहराई में:

1. आइतिहासिक संदर्भ: C में वर्णों को हटाने के लिए `strchr()` फ़ंक्शन पहली बार C89 में शामिल किया गया था।
2. वाईकल्पिक: `strpbrk()` फ़ंक्शन भी इसके लिए उपयोग किया जा सकता है, जो दो स्ट्रिंगों के बीच पहली मिलान वाली स्थिति लौटाता है।
3. कार्यान्वयन विवरण: `strchr()` फ़ंक्शन स्ट्रिंग में वर्ण की पहली उपस्थिति खोजता है। यदि यह वर्ण पाता है, तो यह उसका पॉइंटर लौटाता है।

## और भी देखें:

1.  [Remove all characters in second string which are present in first string](https://www.geeksforgeeks.org/remove-characters-from-the-first-string-which-are-present-in-the-second-string/)
2.  [How to remove a specific character from a string in C programming](https://www.includehelp.com/c/remove-all-occurrences-of-a-character-from-string-in-c.aspx)