---
title:                "वर्तमान तारीख प्राप्त करना"
date:                  2024-01-20T15:13:20.628005-07:00
html_title:           "C: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
करेंट डेट प्राप्त करना मतलब आज की तारीख का डेटा निकालना। प्रोग्रामर्स यह काम लॉग्स, यूजर इंटरैक्शंस, और टाइम-सेंसिटिव फीचर्स के लिए करते हैं। 

## How to: (कैसे करें:)
C में करेंट डेट पाने के लिए `time.h` हेडर फाइल इस्तेमाल होती है। यहाँ एक सिंपल कोड उदाहरण है:

```c
#include <stdio.h>
#include <time.h>

int main() {
    time_t current_time;
    struct tm * time_info;
    char timeString[9]; // space for "HH:MM:SS\0"

    time(&current_time);
    time_info = localtime(&current_time);

    strftime(timeString, sizeof(timeString), "%H:%M:%S", time_info);
    printf("Current Time: %s\n", timeString);

    return 0;
}
```
सैंपल आउटपुट होगा कुछ ऐसा:
```
Current Time: 21:58:07
```

## Deep Dive (गहराई से जानकारी):
`time.h` लाइब्रेरी C में समय और तारीख से जुड़े फंक्शन्स प्रदान करती है। `time` फंक्शन सिस्टम का करेंट टाइम `time_t` टाइप में देता है, और `localtime` इसे `tm` स्ट्रक्चर में बदलता है जो आसानी से पढ़े जा सकें। `strftime` फंक्शन इस डेटा को स्ट्रिंग में फॉर्मेट करता है। इतिहास में पहले कई दूसरे तरीके थे जैसे `gettimeofday` लेकिन `time` की सादगी और क्रॉस-प्लेटफॉर्म सपोर्ट ने इसे ज्यादा लोकप्रिय बना दिया।

अल्टरनेटिव्स में, सी++ में `chrono` लाइब्रेरी और Java में `java.util.Date` वैसे ही काम करते हैं। C में भी कुछ ओएस-स्पेसिफिक API होते हैं जो सीधे सिस्टम क्लॉक से डेटा निकालते हैं।

## See Also (देखें भी):
- The GNU C Library manual on Time: https://www.gnu.org/software/libc/manual/html_node/Time.html
- Microsoft C Run-Time Libraries (CRT) - Date and Time: https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/time-functions?view=msvc-160
- POSIX strftime function reference: https://pubs.opengroup.org/onlinepubs/009695399/functions/strftime.html