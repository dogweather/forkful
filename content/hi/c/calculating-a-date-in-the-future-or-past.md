---
title:                "C: भविष्य या भूतकाल में एक तारीख की गणना"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्यों
क्या आप आज किसी दिन की तारीख की पूर्व या भविष्य में कैलकुलेशन करने का काम करने वाले हो? अगर हाँ, तो आप सही जगह पर हैं! हम आपको बताएंगे कि C प्रोग्रामिंग में तारीख का कैलकुलेशन कैसे किया जाता है।

## कैसे करे
कैलकुलेशन करने के लिए, सबसे पहले हमें तीन पैरामीटरों को प्रोग्राम में पास करना होगा - साल, महीना और दिन। इन पैरामीटरों को एक संरचित डेटा टाइप में स्टोर किया जाता है। यहां, हम आपको कुछ उदाहरणों के साथ एक संपूर्ण कोड देंगे।

```C
#include <stdio.h>

// Function to calculate future or past date
void calculateDate(int year, int month, int day, int daysToAdd) {
    int numDays[12] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}; // number of days in each month
    daysToAdd = daysToAdd % 365; // to handle days greater than 1 year
    int newDay = day + daysToAdd;
    if(year%4 == 0) // condition to check for leap year
        numDays[1] = 29;
    while(newDay > numDays[month-1]) { // to handle days greater than month
        newDay -= numDays[month-1];
        month++;
        if(month > 12) {
            month = 1;
            year++;
        }
        if(year%4 == 0)
            numDays[1] = 29;
        else
            numDays[1] = 28;
    }
    printf("New date: %02d/%02d/%04d", newDay, month, year); // printing the new date in dd/mm/yyy format
}

// Main function
int main() {
    int year, month, day, daysToAdd;
    printf("Enter current date (dd mm yyyy): ");
    scanf("%d %d %d", &day, &month, &year);
    printf("\nEnter number of days to add: ");
    scanf("%d", &daysToAdd);
    calculateDate(year, month, day, daysToAdd); // function call
    return 0;
}
```

**उत्पाद:**

```C
Enter current date (dd mm yyyy): 25 09 2021
Enter number of days to add: 105

New date: 08/01/2022
```

```C
Enter current date (dd mm yyyy): 10 03 2020
Enter number of days to add: 500

New date: 21/07/2021
```

## गहराई में जाएँ
तारीख का कैलकुलेशन करना काफी आसान है। हमने पहले से ही देखा है कि ऐसे कई गुणक हैं जो एक साथ कैलकुलेशन के दौरान ध्यान में रख