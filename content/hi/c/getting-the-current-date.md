---
title:    "C: वर्तमान तिथि प्राप्त करना"
keywords: ["C"]
---

{{< edit_this_page >}}

# क्यों
नमस्ते, दोस्तों! आज हम बात करेंगे C programming में वर्तमान तारीख कैसे प्राप्त की जाए। यह जानने के लिए की गई कई वजहें हैं। जिसमें से कुछ हम नीचे देखेंगे।

# कैसे
```C
#include <stdio.h>
#include <time.h>

int main()
{
    // स्थानिक समय लेकर तारीख और समय प्राप्त करें
    time_t t = time(NULL);
    struct tm *current_time = localtime(&t);

    // तारीख प्राप्त करें
    int day = current_time->tm_mday;
    int month = current_time->tm_mon + 1;
    int year = current_time->tm_year + 1900;

    // समय प्राप्त करें
    int hour = current_time->tm_hour;
    int minutes = current_time->tm_min;
    int seconds = current_time->tm_sec;

    // प्रिंट करें
    printf("आज की तारीख: %d/%d/%d\n", day, month, year);
    printf("स्थानिक समय: %d:%d:%d\n", hour, minutes, seconds);

    return 0;
}
```
उपरोक्त कोड को समझने के लिए आपको कुछ प्रोग्रामिंग की जानकारी की आवश्यकता होगी, लेकिन मैं आपको कुछ सामान्य अवधारणाएं दे सकता हूं। पहले, हम आईसीएस लाइब्रेरी को इंग्लिश राज्यों का समय लेने के लिए #include करते हैं। फिर, तारीख और समय को स्थानिक समय के साथ वापस लेने के लिए "time_t" और "struct tm" का उपयोग किया गया है। अंत में, हम प्रिंट करते हैं परिणाम। फिर, आपको C programming की विशेषाधिकार जानकारी की जरूरत होगी।

# गहराई तक
C programming में, "time.h" लाइब्रेरी से आप प्राप्त कर सकते हैं साल, महीने, कर्नियों, दिन आदि दिनांक और समय जैसे भूमिका। आपको time_t टाइप के भीतर समय-समय प्राप्त करना होगा। आपको इसका उपयोग करना होगा C कोड को स्विच हिंदा में ब