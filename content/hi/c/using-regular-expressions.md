---
title:                "C: नियमित अभिव्यक्तियों का उपयोग करना"
simple_title:         "नियमित अभिव्यक्तियों का उपयोग करना"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## क्यों

रेगुलर एक्सप्रेशन्स का उपयोग करने का कारण है कि यह आसानी से स्ट्रिंग्स को खोजने और प्रोसेस करने की अनुमति देता है। यह प्रोग्रामिंग में देखने को बहुत बारीक और समझने में आसान होता है।

## कैसे करें

```C
#include <stdio.h>
#include <string.h>
#include <regex.h>

int main(void)
{
    char string[] = "Hello, World!";
    regex_t regex;

    // Compile the regular expression
    int ret = regcomp(&regex, "Hello", 0);
    if(ret)
    {
        printf("Could not compile regex\n");
        return 1;
    }

    // Execute the regular expression
    ret = regexec(&regex, string, 0, NULL, 0);
    if(!ret)
    {
        printf("String matches regex\n");
    }
    else if(ret == REG_NOMATCH)
    {
        printf("String does not match regex\n");
    }
}
```

इस उदाहरण में, हम एक स्ट्रिंग को "Hello" रेगुलर एक्सप्रेशन के साथ मिलाने का प्रयास कर रहे हैं। `regcomp` फंक्शन द्वारा हम एक रेगुलर एक्सप्रेशन को परिकलित करते हैं और `regexec` द्वारा हम एक स्ट्रिंग को उस रेगुलर एक्सप्रेशन के साथ मिलाते हैं। इस तरह से, हम देख सकते हैं कि स्ट्रिंग रेगुलर एक्सप्रेशन के साथ मिलती है या नहीं।

## गहराई में जाओ

रेगुलर एक्सप्रेशन्स को प्रोग्रामिंग में इस्तेमाल करने के लिए, आपको इसे समझने की आवश्यकता होती है। रेगुलर एक्सप्रेशन्स में विभिन्न पैटर्न होते हैं जो कि एक स्ट्रिंग को खोजने और प्रोसेस करने में उपयोगी होते हैं। आप अपनी जरूरत के अनुसार अपने रेगुलर एक्सप्रेशन्स को तैयार कर सकते हैं और स्ट्रिंग को मिलाने के लिए विभिन्न फंक्शन का उपयोग कर सकते हैं। रेगुलर एक्सप्रेशन्स को समझने और उनके स