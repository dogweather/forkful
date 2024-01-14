---
title:    "C: השוואת שתי תאריכים"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## למה

בתכנות בשפת C, קיבלתם לעבוד עם תאריכים יכול להיות קשה. השוואת שני תאריכים היא משימה נפוצה וחשובה, שעשויה להיות מאתגרת למתכנתים במתכנתים החדשים ביותר. הכתבה הזאת תזכיר לכם במה התרגלנו עד כה עם תאריכים בתכנון C, ואיך אנו משווים שני תאריכים בקוד שלנו.

## כיצד

ניתן להשתמש במגוון מתודות כדי לשוות שני תאריכים בתכנות בשפת C. הנה כמה דוגמאות של קוד כדי להדגים איך לעשות זאת:

```
#include <stdio.h>

int main()
{
    // Define the first date
    int day1 = 1;
    int month1 = 1;
    int year1 = 2021;
    
    // Define the second date
    int day2 = 31;
    int month2 = 12;
    int year2 = 2021;
    
    // Compare the dates
    if (year1 == year2 && month1 == month2 && day1 == day2) {
        printf("The two dates are equal.");
    }
    else if (year1 > year2) {
        printf("The first date is later.");
    }
    else if (year2 > year1) {
        printf("The second date is later.");
    }
    else if (month1 > month2) {
        printf("The first date is later.");
    }
    else if (month2 > month1) {
        printf("The second date is later.");
    }
    else if (day1 > day2) {
        printf("The first date is later.");
    }
    else {
        printf("The second date is later.");
    }
    
    return 0;
}
```

תוצאה:

```
The second date is later.
```

בקוד זה, אנו משווים את תאריך הראשון עם תאריך השני באמצעות התנאי התואם לכל אחת מהתאריכים. אם נזכור את המחשבה של המחשב דוחף את המערך התאריך כמשתנים, נוכל לראות כי מתכנת C דרוש לנו להשוות את ערכי התאריך העברה המוכנים על התנאי שלכם. השוואת שני תאריכים כלל אינה מאתגרת, אבל ייתכן שיהיו מצבים שבהם תזכורת לתכנית שאתם כנל כמקומות כדי להשוות שני תאריכים בקוד שלכם.

## Deep Dive

ישנם גם דרכים נוספות להשוות שני תא