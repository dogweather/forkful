---
title:                "स्ट्रिंग को कैपिटलाइज़ करना"
date:                  2024-01-19
simple_title:         "स्ट्रिंग को कैपिटलाइज़ करना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

String को capitalize करने का मतलब है हर शब्द के पहले अक्षर को बड़ा (uppercase) करना। Programmers इसे इसलिए करते हैं ताकि text औपचारिक और पढ़ने में आसान लगे, जैसे कि titles या headings में।

## How to: (कैसे करें:)

C में string को capitalize करने के लिए, आप `toupper` function का इस्तेमाल कर सकते हैं। यहाँ एक उदाहरण है:

```C
#include <stdio.h>
#include <ctype.h>

void capitalize(char *s) {
    int i = 0;
    int was_space = 1; // True if the previous character was a space or the string just started
    
    // Loop through each character
    while (s[i]) {
        if (was_space && islower(s[i])) { 
            s[i] = toupper(s[i]);
        }
        was_space = isspace(s[i]);
        i++;
    }
}

int main() {
    char str[] = "hello, world! नमस्ते दुनिया!";
    capitalize(str);
    printf("Capitalized: %s\n", str);
    
    return 0;
}
```

Sample Output:
```
Capitalized: Hello, World! नमस्ते दुनिया!
```
यहाँ, `capitalize` function हर शब्द के पहले अक्षर को बड़ा कर देता है।

## Deep Dive (गहराई से जानकारी)

पुराने समय से ही capitalization का काफी महत्व रहा है - हाथ से लिखे ग्रंथों में भी और printing के दौरान भी। 

C में, `toupper` और `tolower` functions पारंपरिक हैं जो `<ctype.h>` header file में मिलते हैं और इनकी मदद से characters को बदलना संभव होता है। लेकिन इन functions को सही तरीके से इस्तेमाल करना महत्वपूर्ण है, खासकर जब non-ASCII characters involved हों जैसे कि Unicode characters। 

पारंपरिक `toupper` function वाला तरीका ASCII characters के लिए तो ठीक है, पर Unicode characters के साथ सही ढंग से काम नहीं करता। Unicode के लिए, आपको विस्तृत library functions का इस्तेमाल करना पड़ सकता है, जैसे कि `wchar_t` type और `wctype.h` header file के functions। 

साथ ही, प्रोग्राम की efficiency बेहतर बनाने के लिए loops और conditions को सही ढंग से structure करना चाहिए।

## See Also (और जानकारी के लिए)

- [C Standard Library - ctype.h](https://www.cplusplus.com/reference/cctype/)
- [ASCII Table and Description](https://www.asciitable.com/)
- [GNU C Library: Character Handling](https://www.gnu.org/software/libc/manual/html_node/Character-Handling.html)
- [Unicode Standard](https://home.unicode.org/)
