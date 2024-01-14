---
title:    "C: नियमित अभिव्यक्तियों का उपयोग"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## क्यूः

रेगुलर एक्सप्रेशन (regular expressions) का उपयोग उन लोगों द्वारा किया जाता है जो पाठ विश्लेषण (text analysis) के साथ काम करते हैं, जैसे कि स्ट्रिंग मिलान (string matching), रीप्लेसमेंट (replacement), और वैधिकता की जांच (validation).

## कैसे करें

```C
#include <stdio.h>
#include <regex.h>

int main() {
  regex_t regex;
  char string[] = "Hello, World!";
  char pattern[] = "Hello";

  // रेगुलर एक्सप्रेशन को कंपाइल करें
  regcomp(&regex, pattern, REG_EXTENDED);

  // पैटर्न मिलान करें
  int result = regexec(&regex, string, 0, NULL, 0);

  // परिणाम मिलान
  if (result == 0) {
    printf("पाठ विश्लेषण के लिए रेगुलर एक्सप्रेशन उपयोगी हो सकता है!");
  } else {
    printf("रेगुलर एक्सप्रेशन नहीं मिला।");
  }

  return 0;
}
```

आउटपुट:
```
पाठ विश्लेषण के लिए रेगुलर एक्सप्रेशन उपयोगी हो सकता है!
```

## गहराई में जाएं

रेगुलर एक्सप्रेशन का उपयोग करके, आप टेक्स्ट में फिल्टरिंग (filtering) कर सकते हैं, स्ट्रिंग वैधिकता की जांच (validation) कर सकते हैं, अनुक्रमणिका (sorting) कर सकते हैं और भी बहुत कुछ कर सकते हैं। रेगुलर एक्सप्रेशन अधिक शक्तिशाली बनाता है जो टेक्स्ट प्रोसेसिंग (text processing) को आसान बनाता है।

## देखें भी

- [यूनिक्स में रेगुलर एक्सप्रेशन का विस्तृत गाइड](https://www.gnu.org/software/libc/manual/html_node/Regex-Tutorial.html)
- [जावा के लिए रेगुलर एक्सप्रेशन का उपयोग](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)
- [PHP में रेगुलर एक्सप्रेशन केस स्टडी](https://www.php.net/manual/en/function.preg-match.php)