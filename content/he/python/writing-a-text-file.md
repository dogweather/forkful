---
title:    "Python: כתיבת קובץ טקסט"
keywords: ["Python"]
---

{{< edit_this_page >}}

## למה
כתיבת קובץ טקסט היא פעולה חשובה בתוך תהליך התכנות של שפת פייתון. למרבה המקרים, קבצי טקסט מיוצרים ונעדרים באופן אוטומטי כחלק מריצת קוד פייתון.

## איך לבצע את הפעולה
ניתן לכתוב קובץ טקסט בפייתון באמצעות פקודת הפתיחה והסגירה של קובץ עם השימוש בפונקציות המתאימות לכתיבה וקריאה של טקסט. לדוגמה:

```Python
# פתיחת קובץ טקסט לכתיבה
file = open("file.txt", "w")

# כתיבת תוכן לקובץ
file.write("זה טקסט שיודפס לקובץ")

# סגירת הקובץ
file.close() 
```
תוכלו למצוא תוצאות משונות בקובץ בהתאם לפעולות הכתיבה והקריאה שאתם מבצעים על הטקסט.
ניתן לקרוא על פקודת "open" ופונקציות הכתיבה והקריאה בתיעוד הרשמי של פייתון.

## שוטף בעומק
כל טקסט שאתם כותבים בפייתון נשמר בתוך משתנה מסוג סטרינג (string), ועל מנת לשמור אותו בקובץ טקסט יש ליצור את הקובץ באמצעות הפקודה open. בנוסף, יש לסגור את הקובץ באמצעות הפקודה close כדי לוודא שכל המידע נשמר והקובץ אינו נשאר פתוח ומצביע לאותו מקום בכתיבת טקסט אחר.

## ראו גם
- [מדריך רשמי על כתיבת קבצי טקסט בפייתון](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [מדריך בעברית לכתיבת קובץ טקסט בפייתון](https://www.aleph0.info/py_opening-writing-files)
- [מדריך על סיבוכיות הכת