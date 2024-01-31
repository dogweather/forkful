---
title:                "पाठ खोजना और बदलना"
date:                  2024-01-20T17:58:22.781894-07:00
model:                 gpt-4-1106-preview
simple_title:         "पाठ खोजना और बदलना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

टेक्स्ट सर्चिंग और रिप्लेसिंग, मतलब टेक्स्ट की खोज करना और उसे बदलना। प्रोग्रामर्स इसे गलतियाँ ठीक करने, डाटा अपडेट करने और कोड को दोहराव से बचाने के लिए करते हैं।

## How to (कैसे करें):

सी प्रोग्रामिंग में टेक्स्ट सर्च और रिप्लेस का एक उदाहरण देखिए -

```c
#include <stdio.h>
#include <string.h>

void searchAndReplace(char *source, const char *search, const char *replace) {
    char buffer[1024];
    char *p;
    
    if ((p = strstr(source, search)) == NULL) {
        printf("String not found!\n");
        return;
    }
    
    strncpy(buffer, source, p - source);
    buffer[p - source] = '\0';
    
    snprintf(buffer + (p - source), sizeof(buffer) - (p - source), "%s%s", replace, p + strlen(search));
    strcpy(source, buffer);
    
    printf("Updated String: %s\n", source);
}

int main() {
    char sourceString[1024] = "Hello World!";
    searchAndReplace(sourceString, "World", "There");
    return 0;
}
```

Sample Output:

```
Updated String: Hello There!
```

## Deep Dive (गहराई में):

टेक्स्ट सर्च और रिप्लेस एक सामान्य लेकिन महत्वपूर्ण कार्य है। सी में `strstr` और `strcpy`, `strncpy` जैसे फंक्शन्स रिप्लेसमेंट के लिए उपयोगी होते हैं। पुराने जमाने में `sed` या `awk` जैसे टूल्स का इस्तेमाल होता था जो आज भी वैलिड हैं। एक उपयोगी ऑल्टरनेटिव रेगुलर एक्सप्रेशन्स या "regex" है, जो कि जटिल पैटर्न्स की खोज और रिप्लेसमेंट को आसान करता है।

इंप्लिमेंटेशन में सबसे महत्वपूर्ण बात यह है कि बफर ओवरफ्लो न हो। सुनिश्चित करें कि `buffer` उतना बड़ा हो कि नया टेक्स्ट समा सके।

## See Also (और जानकारी):

- C Standard Library Functions: `strstr`, `strcpy`, `strncpy`, `snprintf` ([cplusplus.com](https://www.cplusplus.com/reference/cstring/))
- Regular Expressions in C (regex.h): [GNU documentation](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html)
- Sed & Awk Tools: [GNU documentation](https://www.gnu.org/software/sed/manual/sed.html) and [Awk Tutorial](https://www.gnu.org/software/gawk/manual/gawk.html)
