---
title:                "C: पैटर्न को मिलाने वाले अक्षरों को हटाना"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## क्यों

इस ब्लॉग पोस्ट में हम आपको बताएंगे कि C प्रोग्रामिंग में पैटर्न को मैच करने वाले अक्षरों को हटाने क्यों जरूरी है।

## कैसे करें

इस काम को करने के लिए हमें एक स्ट्रिंग (चर) और पैटर्न स्ट्रिंग (मात्रा) की आवश्यकता होती है। फिर हम प्रोग्राम लिखते हैं जो दी गई पैटर्न स्ट्रिंग के आधार पर स्ट्रिंग से उसी पैटर्न को मैच करने वाले अक्षरों को हटा देता है। नीचे दिए गए कोड ब्लॉक में आप इसका संकलन देख सकते हैं।

```C
// पैटर्न को मैच करने वाले अक्षरों को हटाएं
#include <stdio.h>
#include <string.h>

void deleteMatchingPattern(char *str, char *pattern) {
    int i, j, index = 0;
    int len = strlen(pattern);

    // स्ट्रिंग से पैटर्न को मैच करने वाले अक्षरों को हटाएं
    for (i = 0; str[i] != '\0'; i++) {
        for (j = 0; j < len; j++) {
            if (str[i+j] != pattern[j]) {
                break;
            }
        }

        if (j == len) { // पैटर्न मिल गया है
            i += len - 1; // पुनरावृत्ति: पैटर्न लंबाई - 1
        } else { // पैटर्न मिल नहीं जाता है
            str[index] = str[i];
            index++;
        }
    }
    str[index] = '\0';
}

int main() {
    char str[100], pattern[100];

    // स्ट्रिंग और पैटर्न स्ट्रिंग प्राप्त करें
    printf("कृपया स्ट्रिंग दर्ज करें: ");
    gets(str);
    printf("कृपया पैटर्न स्ट्रिंग दर्ज करें: ");
    gets(pattern);

    deleteMatchingPattern(str, pattern); // स्ट्रिंग से पैटर्न मिलेंगे
    printf("परिणाम स्ट्रिंग: %s\n", str);

    return 0;
}

##### उपयोगकर्ता आउटपुट:

कृपया स्ट्रिंग दर्ज करें: Hello World
कृपया पैटर्न स्ट्रिंग दर्ज करें: lo
परिणाम स्ट