---
title:                "חישוב תאריך בעתיד או בעבר"
html_title:           "C: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# מחשבת תאריך עתידי/עברי בשפת C

## מה ולמה?
ביכולתנו לחשב תאריך בעתיד או בעבר היא היכולת למצוא תאריך מסוים מבלי לספור את הימים באופן ידני. זה נותן למתכנתים את היכולת להניע יישומים באופן דינאמי בהתאם לתאריך, פונקציה שתכופה ביישומים רבים.

## כיצד למעשה:
חבל הקוד הבא מחשב את התאריך שנייה מעכשיו:

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t now = time(NULL);
    time_t future = now + 2;

    char* c_time_string = ctime(&future);
    printf("The time two seconds in the future will be: %s", c_time_string);

    return 0;
}
```

העתיד המודפס בפלט צפוי להיות כ-2 שניות מתאריך הזמן שבו התוכנה התחילה.

## צלילה עמוקה:
השילוב של הפונקציות `time` ו- `ctime` נפוץ בעידן המודרני של שימוש בשפת תכנות C, אך הן מוצאות את מקורן בתחילת שנות ה-70. אלטרנטיבות כוללות שימוש ב- `gettimeofday` במקרים בהם הדיוק הוא יותר חשוב, או מערכות ניהול זמן מותאמות אישית לישומים ספציפיים כמו תוכנות ניהול פרויקטים.

בראש ובראשונה, חשיבה על התאריך העתידי או העברי היא פשוטה באמצעות שפת תכנות C, שכן דורש להוסיף או להפחית מהאינטגרל 'זמן' כדי להתקבל לתאריך החדש.

## ראו גם:
דף עזרה של `time`: [http://www.cplusplus.com/reference/ctime/time/](http://www.cplusplus.com/reference/ctime/time/)
רמות `ctime`: [http://www.cplusplus.com/reference/ctime/ctime/](http://www.cplusplus.com/reference/ctime/ctime/)
מדריך ל- `gettimeofday`: [https://www.gnu.org/software/libc/manual/html_node/Elapsed-Time.html](https://www.gnu.org/software/libc/manual/html_node/Elapsed-Time.html)