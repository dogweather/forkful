---
title:                "Python: הדפסת פלט תיקונים"
programming_language: "Python"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/printing-debug-output.md"
---

{{< edit_this_page >}}

# למה

הדפסת פלט זיהוי שגיאות היא כלי חשוב לעבודה עם פייתון. באמצעות הדפסת פלט הנתונים וההערות ניתן לזהות את השגיאות ולתקן אותן בקלות. 

# איך לעשות זאת

```python
# דוגמא של הדפסת פלט זיהוי שגיאות
x = 5
y = 0

try:
    result = x/y
except ZeroDivisionError as e:
    print("Error:", e) # עיסוק בהדפסת השגיאה
```
כאן אנו יוצרים משתנה x ומשתנה y ומנסים לחלק את x ב-y. כמו כן, אנו משתמשים במנגנון ה try/except כדי לכובות את השגיאה הקורצת של החלוקה ב-0. על ידי הדפסת השגיאה, אנו יכולים לזהות את הבעיה ולתקן אותה. 

# עיון עמוק

כדי להתקדם עוד יותר בהדפסת פלט זיהוי שגיאות, ניתן להשתמש בפקודת  `print()` עם פרמטר `file=sys.stderr`. דבר זה יציג את הפלט בחלונת הפלט עי ממשתמשים יכולים לראות את השגיאות ולתקן אותן בצורה מהירה יותר. כמו כן, ניתן להשתמש בפקודת `logging` כדי להדפיס פלט זיהות שגיאות בקובץ לוגים במקום במסך. בכך אנו שומרים את המידע על השגיאות לצורך דיווח או ניתוח נוסף. 

# ראה גם

- [Python Debugging with print statement](https://www.digitalocean.com/community/tutorials/how-to-use-the-python-debugger)
- [Logging in Python: An Introductory Tutorial](https://www.digitalocean.com/community/tutorials/how-to-use-python-logging)
- [Debugging and Error Handling in Python](https://realpython.com/python-debugging-pdb/)