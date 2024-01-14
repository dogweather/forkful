---
title:    "C: दो तारीखों का तुलना करना"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्यों

दो तारीखों को तुलना करने से पहले, हमें उन्हें विभिन्न मानों में परिवर्तित करना पड़ता है। यह काम एक सामान्य कार्य हो सकता है, लेकिन यह हमारे प्रोग्राम के लिए जरूरी हो सकता है।

## कैसे करें

```
C
#include <stdio.h>
#include <time.h>

// Function to compare two dates
int compareDates(int day1, int month1, int year1, int day2, int month2, int year2) {
    
    // Convert dates into time structure
    struct tm date1 = { .tm_mday = day1, .tm_mon = month1 - 1, .tm_year = year1 - 1900 };
    struct tm date2 = { .tm_mday = day2, .tm_mon = month2 - 1, .tm_year = year2 - 1900 };
    
    // Convert dates into time_t format
    time_t time1 = mktime(&date1);
    time_t time2 = mktime(&date2);
    
    // Compare the time_t values
    if (time1 < time2) {
        return -1; // Date 1 is before Date 2
    } else if (time1 > time2) {
        return 1; // Date 1 is after Date 2
    } else {
        return 0; // Both dates are same
    }
}

int main() {
    // Input first date
    int day1, month1, year1;
    printf("Enter first date (DD/MM/YYYY): ");
    scanf("%d/%d/%d", &day1, &month1, &year1);
    
    // Input second date
    int day2, month2, year2;
    printf("Enter second date (DD/MM/YYYY): ");
    scanf("%d/%d/%d", &day2, &month2, &year2);
    
    // Compare dates
    int result = compareDates(day1, month1, year1, day2, month2, year2);
    
    if (result == -1) {
        printf("%d/%d/%d is before %d/%d/%d", day1, month1, year1, day2, month2, year2);
    } else if (result == 1) {
        printf("%d/%d/%d is after %d/%d/%d", day1, month1, year1, day2, month2, year2);
    } else {
        printf("%d/%d/%d is equal to %d/%d/%d", day1, month1, year1, day2, month2, year2);
    }
    
    return 0;
}
```

यहां हमने दो छोटे से कार्य किए हैं। पहले, हमने दो समय संरचनाओं में तारीखों को संकलित किया है, और फिर हमने उन्हें time_t फॉर्मेट में बदल दिया है। समय संरचना structure तक पहुंच प्राप्त करने के बाद, हम समय संरचना structure का उपयोग समय संरचना structure को time_t फॉर्मेट में कन्वर्ट करने के लिए करते हैं। कृपया ध्यान दें कि माह के लिए मान - 1 का उपयोग हमारे सम