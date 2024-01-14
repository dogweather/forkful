---
title:    "Python: יצירת קובץ זמני"
keywords: ["Python"]
---

{{< edit_this_page >}}

## למה

יצירת קובץ זמני היא כלי חיוני לכל מתכנת פייתון. היא מאפשרת יצירת קבצים שנמחקים בסופו של דבר ומיועדים לשימוש זמני, כך שניתן להשתמש בהם כדי לאחסן מידע שנדרש רק לזמן מוגבל.

## איך לעשות את זה

### יצירת נתיב זמני

```python
import tempfile

# יצירת נתיב זמני עם פלט ריק
temp_file = tempfile.mktemp()
print(temp_file)
```
פלט: `C:\Users\Username\AppData\Local\Temp\tmp3b4v6gu4`

### יצירת קובץ זמני בספרייה זמנית

```python
import tempfile

with tempfile.TemporaryDirectory() as temp_dir:
    # יצירת קובץ זמני בתוך הספרייה הזמנית
    temp_file = tempfile.NamedTemporaryFile(dir=temp_dir)
    print(temp_file.name)
```
פלט: `C:\Users\Username\AppData\Local\Temp\tmpyx3x12ep\tmpfh5lw3mx`

## כיול עומק

קבצים זמניים מתאפיינים בכך שהם נמחקים או מועברים לתיקייה הזמנית עם סיום התוכנית או הפונקציה שיצרה אותם. מרבית הפעמים, כשמשתמשים בקבצים זמניים, ההיכן בו הם נשמרים לא משונה כל כך. מה שחשוב לדעת הוא שכל קובץ זמני יש לו נתיב שמוכר רק לתהליך שייצר אותו.

## ראה גם:

- [מדריך: יצירת קבצים זמניים בשפת פייתון](https://realpython.com/python-tempfile/)
- [מפתחים עושים דאטא גריסיים: נהלי טקסט לשפת פייתון](https://blog.michalpaserman.com/2016/12/%D7%9E%D7%A4%D7%AA%D7%97%D7%99%D7%9D-%D7%A2%D7%95%D7%A9%D7%99%D7%9D-%D7%93%D7%90%D7%98%D7%90-%D7%92%D7%A8%D7%99%D7%A1%D7%99%D7%99%D7%9D-%D7%A0%D7%94%D7%9C%D7%99-%D7%98%D7%A7%D7%A1%D7%98-%D7%9C%D7%A9%D7%A4%D7%AA-%D7%A4%D7%99%D7%99%D7%AA%D7%95%D7%9F/)