---
title:    "C: भविष्य या भूतकाल में दिनांक की गणना"
keywords: ["C"]
---

{{< edit_this_page >}}

Hindi में C programming blog पोस्ट करना:

## क्यों
दिनांक को भविष्य या भूतकाल में गणना करने के लिए *क्यों* किसी को संलग्न करना पड़ता है, यह केवल 1-2 सेंटेंस में समझाया जाएगा।

## कैसे करें
कोड उदाहरण और "```C ... ```" कोड ब्लॉक्स के भीतर नमूना आउटपुट।

```C
#include <stdio.h>

int main() {
    int currentDay = 5;
    int currentMonth = 9;
    int currentYear = 2021;
    int daysToAdd = 7;

    int futureDay = currentDay + daysToAdd;
    int futureMonth = currentMonth;
    int futureYear = currentYear;

    if (futureDay > 30) {
        futureDay -= 30;
        futureMonth++;
    }

    if (futureMonth > 12) {
        futureMonth = 1;
        futureYear++;
    }

    printf("Future Date: %d/%d/%d", futureDay, futureMonth, futureYear);
    return 0;
}
```

आउटपुट:
Future Date: 12/9/2021

## गहराई में जाएं
भविष्य या भूतकाल में दिनांक की गणना से जुड़े गहराई जानने के लिए। कुछ लोग इस काम को आसान मानते हैं। हालांकि, पहले दिनांक, महीने और वर्षों की गणना के बारे में जानकारी होना जरूरी है।

## See Also
देखें भी (Translated into Hindi):

1. [Date and Time Manipulation in C](https://www.programiz.com/c-programming/c-date-time)
2. [Calculating Future and Past Dates in C](https://www.tutorialspoint.com/how-to-add-or-subtract-days-from-date-time-in-c-programming)
3. [Date and Time Functions in C](https://www.geeksforgeeks.org/date-and-time-functions-in-c-cpp/)
4. [Calculating Date Differences in C](https://stackoverflow.com/questions/26750089/calculating-date-differences-in-c)

आप इन लिंक्स का उपयोग करके और अपनी स्थापित जानकारी का उपयोग करके C में भविष्य या भूतकाल में दिनांक की गणना कर सकते हैं।