---
title:                "रेगुलर एक्सप्रेशन का उपयोग"
date:                  2024-01-19
html_title:           "Bash: रेगुलर एक्सप्रेशन का उपयोग"
simple_title:         "रेगुलर एक्सप्रेशन का उपयोग"

category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

रेगुलर एक्सप्रेशंस, यानी कि सामान्य व्यंजक, पाठ को प्रोसेस करने का एक शक्तिशाली तरीका हैं। प्रोग्रामर्स इनका इस्तेमाल पैटर्न मिलान, खोज और डेटा संशोधन के लिए करते हैं।

## How to: (कैसे करें:)

```C
#include <stdio.h>
#include <regex.h>

int main() {
    regex_t regex;
    int return_value;
    char *pattern = "^शु.+म$";
    char *test_string = "शुभकामनाएं";

    // रेगुलर एक्सप्रेशन तैयार करें
    return_value = regcomp(&regex, pattern, REG_EXTENDED);

    // टेस्ट करें
    return_value = regexec(&regex, test_string, 0, NULL, 0);

    if (return_value == 0) {
        printf("पैटर्न मिला: %s\n", test_string);
    } else {
        printf("पैटर्न नहीं मिला.\n");
    }

    // रेगुलर एक्सप्रेशन मुक्त करें
    regfree(&regex);
    
    return 0;
}
```

```output
पैटर्न मिला: शुभकामनाएं
```

## Deep Dive (गहराई से जानकारी)

रेगुलर एक्सप्रेशन्स का विकास केन थॉम्पसन ने किया था और यह सबसे पहले Unix में प्रयुक्त हुए। विकल्प के रूप में, आप स्ट्रिंग खोज फंक्शन्स का इस्तेमाल कर सकते हैं, पर वे उतने शक्तिशाली नहीं होते। C लाइब्रेरी 'regex.h' प्रोग्रामिंग के लिए POSIX रेगुलर एक्सप्रेशन API प्रदान करती है।

## See Also (इसे भी देखें)

- [GNU C Library Manual: Regular Expressions](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html)
- [RegexOne: Learn Regular Expressions](https://regexone.com/)
