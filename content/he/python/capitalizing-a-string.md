---
title:    "Python: כתיבת מחרוזת באותיות רישיות"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# מדוע

אתם כנראה מתרגשים ללמוד על איך משתנה אחד קטן בפייתון יכול להיות מפתח חשוב בכתיבת קוד. כתיבת קוד באופן מונכרי ומסודר יכול להיות מאתגר ועיסוק סובלני, אבל שימוש בכלי פשוט זה יכול לעזור לכם לשמור על הקוד שלכם נקי ומסודר.

## איך לעשות

הנה דוגמה קוד פשוט שמדגיש איך להשתמש בפעולה "capitalize" כדי להמיר את האות הראשונה של מחרוזת לאות גדולה:

```Python
text = "hello world"
print(text.capitalize())
```
הפלט:

```
Hello world
```

כאן אנו בעצם משנים את האות הראשונה של המחרוזת לאות גדולה ומדגישים איפה ישנו שינוי קטן, אבל חשוב.

## הסתכלות עמוקה

כדי להבין את איך הפעולה "capitalize" עובדת בקוד בפייתון, ניתן להשתמש בפונקציה "help" כדי לקבל מידע נוסף על הפעולה. נקלוט "help(str.capitalize)" כדי להציג פלט שמציג מידע על הפעולה ואת דרכי השימוש:

```Python
help(str.capitalize)
```

הפלט:

```
Help on method_descriptor:

capitalize(...)
    S.capitalize() -> str
    
    Return a capitalized version of S, i.e. make the first character
    have upper case and the rest lower case.
```

ניתן לראות בפלט שהפעולה "capitalize" יוצרת גרסה חדשה של המחרוזת עם האות הראשונה מופעלת כאות גדולה, וכך משנה את כתיבת המחרוזת המקורית.

# ראה גם

- [פייתון תיעוד רשמי על capitalize](https://docs.python.org/3/library/stdtypes.html#str.capitalize)
- [מדריך תיעוד רשמי על פריסת כתיבת קוד עם פייתון](https://www.python.org/dev/peps/pep-0008/#capitalized-names)