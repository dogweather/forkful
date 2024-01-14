---
title:                "C: भविष्य या अतीत में दिनांक की गणना करना ।"
simple_title:         "भविष्य या अतीत में दिनांक की गणना करना ।"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

क्यों: कोई व्यक्ति भविष्य या भूतकाल में तारीख की गणना करने में सक्रिय होना क्यों करेगा, इसका व्याख्यान केवल 1-2 वाक्यों में।

## कैसे करें:

```
#include<stdio.h>
#include<stdlib.h>

int main()
{
    int day, month, year;
    int future_day, future_month, future_year;
    int past_day, past_month, past_year;

    // प्रभावों की सभी प्राथमिक भराइएं दर्ज करें
    printf("आज का दिन, महीना और साल (उदा: 9 10 2021): ");
    scanf("%d %d %d", &day, &month, &year);

    // भविष्य की तारीख का पता लगाएं
    printf("भविष्य की तारीख की संख्या: ");
    scanf("%d", &future_day);

    // भविष्य की तारीख की गणना करें
    future_month = month + (future_day / 30);
    future_year = year + (future_month / 12);
    future_day = future_day % 30;
    future_month = future_month % 12;

    // पश्चात्तम की तारीख का पता लगाएं
    printf("पश्चात्तम की तारीख की संख्या: ");
    scanf("%d", &past_day);

    // पश्चात्तम की तारीख की गणना करें
    past_month = month - (past_day / 30);
    past_year = year - (past_month /12);
    past_day = past_day % 30;
    past_month = past_month % 12;

    // परिणाम मुद्रित करें
    printf("%d दिन बाद की तारीख: %d/%d/%d \n", future_day, future_month, future_year);
    printf("%d दिन पहले की तारीख: %d/%d/%d \n", past_day, past_month, past_year);

    return 0;
}
```

प्रोग्राम लागू करने पर निम्नलिखित आउटपुट प्राप्त करें:
```
आज का दिन, महीना और साल (उदः: 9 10 2021): 18 10 2021
भविष्य की तारीख की संख्या: 25
पश्चात्तम की तारीख की संख्या: 12
25 दिन बाद की तारीख: 12/11/2021
12 दिन पहले की तारीख: 6/10/2021
```

## गहराई में जाएं:

भविष्य या पश्चात्तम की तारीख की गणना करना बहुत ही उपयोगी हो सकता है। इससे आप