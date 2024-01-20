---
title:                "स्ट्रिंग का अंतर्कलन"
html_title:           "Arduino: स्ट्रिंग का अंतर्कलन"
simple_title:         "स्ट्रिंग का अंतर्कलन"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/interpolating-a-string.md"
---

{{< edit_this_page >}}

Title: वाक्यांश संवेदन (String Interpolation) की C में प्रोग्रामिंग

## क्या और क्यों?

वाक्यांश संवेदन एक तरीका है जिससे किसी प्रोग्राम में वाक्यांशों के बीच मान्यताओं को जोड़ा जा सकता है। प्रोग्रामर्स इसे वाक्यांश फॉर्मेटिंग को आसान और पठनीय बनाने के लिए करते हैं।

## कैसे करें:

ताज़ा संस्करण (C11) के C प्रोग्रामिंग में, हम फ़ंक्शन `snprintf` का उपयोग कर विस्तार संवेदन करते हैं। 

```c
#include <stdio.h>

int main() {
    char name[] = "John Doe";
    char buffer[50];

    snprintf(buffer, sizeof(buffer), "नमस्ते %s", name);
    printf("%s", buffer);

    return 0;
}
```
उदाहरण का निष्कर्ष:
```
नमस्ते John Doe
```

## गहराई में:

वाक्यांश संवेदन के ऐतिहासिक संदर्भ में, यह पहले ब्रेकेट के माध्यम से टर्मिनल कन्डिशन भाषा (TCL) में साधारित था। 

विकल्पों के रूप में, कुछ भाषाओं, जैसे कि Python और JavaScript, में अंदरूनी वाक्यांश संवेदन समर्थित होते हैं, जो C की अपेक्षा अधिक रचनात्मकता प्रदान करते हैं। 

संविष्ठता के हिसाब से, `snprintf` एक कड़ी जांच करता है कि वाक्यांश को बफ़र में सही तरीके से समायोजित किया जा रहा है और अधिक समय वाले वाक्यांशों के लिए योग्यता काट देता है।

## अन्य संदर्भ:

1. Wikipedia पर वाक्यांश संवेदन: https://en.wikipedia.org/wiki/String_interpolation
2. C लैंग्वेज 'snprintf' फ़ंक्शन: https://en.cppreference.com/w/c/io/fprintf
3. वाक्यांश संवेदन की पायथन उदाहरण: https://www.programiz.com/python-programming/string-interpolation