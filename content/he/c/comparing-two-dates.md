---
title:                "השוואת שתי תאריכים"
html_title:           "C: השוואת שתי תאריכים"
simple_title:         "השוואת שתי תאריכים"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## מה ולמה?
השוואת שתי תאריכים היא פעולה שנעשית על ידי מפתחי תכנות כדי לבדוק האם שני תאריכים זהים או אם תאריך אחד מגיע לפני השני. השוואת תאריכים חשובה כשמתעסקים עם תאריכים בתוכניות שלנו, כדי שנוכל לבדוק אם הנתונים שאנחנו מקבלים הם נכונים ותואמים את הציפיות שלנו.

## כיצד לעשות זאת?
```C
#include <stdio.h>
#include <string.h>
#include <time.h>

// פונקציה להשוואת שני תאריכים
int compareDates(struct tm *date1, struct tm *date2){
    if(date1->tm_year < date2->tm_year) return -1;
    else if(date1->tm_year > date2->tm_year) return 1;
    else if(date1->tm_mon < date2->tm_mon) return -1;
    else if(date1->tm_mon > date2->tm_mon) return 1;
    else if(date1->tm_mday < date2->tm_mday) return -1;
    else if(date1->tm_mday > date2->tm_mday) return 1;
    else return 0; // שני התאריכים זהים
}

int main(){
    // משתנים לשני התאריכים
    struct tm date1, date2;
    
    // נקבע את התאריכים שנשווה
    date1.tm_year = 2021;
    date1.tm_mon = 5;
    date1.tm_mday = 12;
    
    date2.tm_year = 2021;
    date2.tm_mon = 5;
    date2.tm_mday = 13;

    // הדפסת התוצאה - שוויון
    if(compareDates(&date1, &date2) == 0) printf("התאריכים זהים\n");
    // הדפסת התוצאה - date1 מוקדם יותר
    else if(compareDates(&date1, &date2) == -1) printf("התאריך הראשון מוקדם יותר מהתאריך השני\n");
    // הדפסת התוצאה - date2 מוקדם יותר
    else if(compareDates(&date1, &date2) == 1) printf("התאריך השני מוקדם יותר מהתאריך הראשון\n");
}
```

רצוי לקחת בחשבון כי תאריכים וזמנים מתוזמנים ומשתנים בכל שנייה ולכן חשוב לבדוק גם את השניות כדי לוודא שהתאריך המדויק נשלח לתכנית.

## העדר פרטים נוספים
השוואת שני תאריכים כבר מאוד מפורסמת ומשמשת גם בתחום המחשבים וגם בתחום התכנות. קיימות גם כלים אוטומטיים שמבצעים השוואה ולא ממצלמים אותנו לבדוק בעצמנו.

## ראו גם
למידע נוסף על השוואת תאריכים וזמנים ב-C, ניתן לקרוא את הקוד המקורי של הספריה התקנית time.h.

לקריאה נוספת על השוואה על ידי שוויון, אתר ויקיפדיה מסביר בפירוט את הנושא: https://he.wikipedia.org/wiki/%D7%94%D7%A1%D7%92%D7%9C_%D7%94%D7%A8%D7%90%D7%A9%D7%95%D7%9F.