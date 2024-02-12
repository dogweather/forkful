---
title:                "כתיבת קובץ טקסט"
date:                  2024-02-03T19:27:35.780680-07:00
model:                 gpt-4-0125-preview
simple_title:         "כתיבת קובץ טקסט"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

כתיבת קובץ טקסט ב-Bash מאפשרת לך לאוטמט אחסון נתונים, רישום, הגדרות תצורה ועוד. זוהי מיומנות בסיסית לתסריטי קונסולה, שמאפשרת למתכנתים לשמור את פלט הפקודות, ביצועי תסריטים, או קלט מהמשתמש לדיווח, עיבוד, או ביצועים עתידיים.

## איך לעשות:

Bash מספקת שיטות ישירות לכתיבה לקובץ. הנפוצות ביותר הן שימוש באופרטורים להפניית פלט (`>`, `>>`) והפקודה `tee`. הנה מבט מהיר על שתי הטכניקות.

באמצעות הפניית פלט, ניתן לכתוב פלט ישירות לקובץ. האופרטור `>` כותב תוכן לקובץ, מחליף אותו אם הוא כבר קיים, בעוד ש-`>>` מוסיף לקובץ קיים מבלי למחוק את תוכנו.

```bash
# כתיבה לקובץ עם >
echo "Hello, World!" > myfile.txt

# הוספה לקובץ עם >>
echo "This is a new line." >> myfile.txt
```

אם תבדוק את תוכן `myfile.txt` לאחר הרצת הפקודות לעיל, תמצא:

```
Hello, World!
This is a new line.
```

הפקודה `tee` שימושית כאשר אתה רוצה לכתוב לקובץ וגם לראות את הפלט על המסך (stdout) בו זמן אמת. כברירת מחדל, `tee` מחליף את הקובץ, אך עם הדגל `-a`, הוא מוסיף לקובץ.

```bash
# כתיבה והצגה באמצעות tee
echo "Hello, again!" | tee myfile.txt

# הוספה והצגה באמצעות tee -a
echo "Adding another line." | tee -a myfile.txt
```

לאחר הרצת אלו, `myfile.txt` יציג:

```
Hello, again!
Adding another line.
```

עוד ש-Bash עצמה מספקת יכולות ניהול קבצים חזקות באמצעות הפנייה ופקודות כמו `tee`, מניפולציה נוספת או תרחישים מורכבים יותר עשויים לדרוש קריאה לכלים חיצוניים או שפות תסריט כמו Awk, Sed, Python, שמציעות פונקציות עיבוד טקסט מתקדמות יותר. עם זאת, למשימות כתיבת קבצים ישירות, השיטות הנ"ל הן לגמרי מספיקות ונפוצות באופן רחב.