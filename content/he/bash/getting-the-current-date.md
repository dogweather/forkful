---
title:                "קבלת התאריך הנוכחי"
html_title:           "C#: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

# מדריך לתכנות Bash: קבלת התאריך הנוכחי
מאת: [שם המחבר]

## איך ולמה?
הגבלת התאריך הנוכחי ב-Bash היא פעולה שבאמצעותה אנו מיישמים את התאריך והשעה שבהם מתבצע הסקריפט. זה שימושי לחיבור יומנים, תיוג הקבצים או כל מטרה אחרת שדורשת זמנים.

## איך לעשות:
להלן דוגמא של איך להניח את התאריך הנוכחי ב-Bash:
```Bash
תאריך=$(date "+%d/%m/%Y")
echo $תאריך
```
אם תריצו את הסקריפט הזה, תקבלו את התאריך הנוכחי. נניח שהיום הוא ה-23 בפברואר 2022, הפלט יהיה:

```
23/02/2022
```

## צלילה עמוקה
הבש אינו מקצועי כמו שפות תכנות אחרות, אך הוא יעיל מאוד באופן ספציפי זה. זה הרבה מעבר ליכולת הפשוטה להדפיס את התאריך - ניתן לבחור במגוון תבניות וסוגי מידע וזמן. בחירות אלה יכולות לשנות בהתאם למערכת ההפעלה, אך השורה התחתונה היא ש`date` הוא כלי חזק מאוד עבור באש.

אלטרנטיבה נוספת לשאילתא היא Perl:

```Bash
perl -e 'print scalar(localtime())'
```

נדירה אך הכרחית לשים לב: לא כל המערכות מכילות `date`. על מנת לשמור למערכות אלו, ניתן להשתמש ב-Perl או בשפת סקריפט אחרת אם היא מותקנת.

## ראה גם
1. [מדריך GNU ל-date](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
2. [מדריך Bash מתקדם](https://tldp.org/LDP/abs/html/)
3. [מדריך ל-Perl](https://www.perl.org/books/beginning-perl/)
4. [פורום פיתוח Bash](https://www.linuxquestions.org/questions/programming-9/)