---
title:                "C: दो तारीखों की तुलना"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्यों

कभी-कभी हमें दो तिथियों को तुलना करनी पड़ती है, जैसे की किसी ईवेंट की तारीख या स्पेशल दिन की तारीख। ऐसी स्थिति में हमें इन तिथियों के बीच में अंतर जानना बहुत महत्वपूर्ण हो सकता है। यह तिथियों का तुलनात्मक विश्लेषण तब भी आवश्यक हो सकता है जब हमें दो तिथियों के बीच दिनों, महीनों और सालों में अंतर का पता लगाना हो। तो चलिए जानते हैं कि इस विषय को कैसे हल किया जाता है।

## कैसे करें

दोबारा तिथियों को तुलना करने के लिए, हमें C प्रोग्रामिंग भाषा का उपयोग करना पड़ेगा। यह तरीका बहुत सरल है और आप इसे आसानी से समझ सकते हैं। तो आइए एक उदाहरण के साथ इस विषय को समझते हैं।

```C
#include <stdio.h>

int main()
{
    int date1, month1, year1; // पहली तारीख के दिन, महीना और साल
    int date2, month2, year2; // दूसरी तारीख के दिन, महीना और साल

    printf("Enter first date (DD MM YYYY): "); // पहली तारीख दर्ज करें
    scanf("%d %d %d", &date1, &month1, &year1);

    printf("Enter second date (DD MM YYYY): "); // दूसरी तारीख दर्ज करें
    scanf("%d %d %d", &date2, &month2, &year2);

    // दो तारीखों के बीच अंतर की गणना
    int days, months, years;

    if (date2 >= date1) {
        days = date2 - date1; // दिनों का अंतर
        months = month2 - month1; // महीनों का अंतर
        years = year2 - year1; // सालों का अंतर
    }
    else {
        days = date1 - date2; // दिनों का अंतर
        months = month1 - month2; // महीनों का अंतर
        years = year1 - year2; // सालों का अंतर
    }

    // दोनों तारीखों के बीच