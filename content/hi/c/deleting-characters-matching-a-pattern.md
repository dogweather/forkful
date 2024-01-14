---
title:    "C: पैटर्न से मिलते हुए अक्षरों को हटाना"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# क्यों

एक निश्चित पैटर्न से मैच करने वाले अक्षरों को हटाने में काम लेने का *क्यों* कोई रुझान होगा जहां उपयोगकर्ता को केवल निश्चित अक्षरों को हटाने की आवश्यकता होगी।

# कैसे करें

```C
#include<stdio.h>

void deletePattern(char *str, char *pattern) {
  int i, j, k, n, m;
  n = strlen(str);
  m = strlen(pattern);
  
  for (i = 0; i <= n - m; i++) {
    for (j = i, k = 0; k < m; j++, k++) {
      if (str[j] != pattern[k]) {
        break;
      }
    }
    if (k == m) {
      for (j = i, k = k - 1; k >= 0; j++, k--) {
        str[j] = ' ';
      }
    }
  }
}

int main() {
  char str[] = "यह पाठ कुछ गलत अक्षरों के साथ है।";
  char pattern[] = "कुछ";
  
  printf("पूर्व मैसेज: %s\n", str);
  deletePattern(str, pattern);
  printf("नया मैसेज: %s", str);
  
  return 0;
}
```

आउटपुट:

पूर्व मैसेज: यह पाठ कुछ गलत अक्षरों के साथ है।
नया मैसेज: यह पाठ        गलत अक्षरों के साथ है।

# गहराई में

प्रोग्रामिंग में अक्षरों को प्रोसेस करने का सबसे सामान्य तरीका `char` टाइप का उपयोग करना है। हालांकि, वैध स्ट्रिंग प्रोसेसिंग `char` अरे के साथ नहीं हो सकती है क्योंकि `char` केवल एक ही अक्षर को संग्रहीत कर सकता है। इसलिए, अक्षरों को हटाने के लिए हमें उन्हें एक स्ट्रिंग के रूप में संग्रहीत करने की आवश्यकता होती है ताकि हम उन्हें प्रोसेस कर सकें। इस लेख में हमने स्पेशल तरीके से `char` अरे की संख्या पायी गई है जो हमें पैटर्न के आधार पर स्ट्रिंग प्रोसेसिंग करने की अनुमति देती है।

# इसे भी देखें

[Hindi Programming Tutorial: C Strings and String Functions](https://www