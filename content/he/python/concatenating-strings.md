---
title:                "חיבור מחרוזות"
html_title:           "Python: חיבור מחרוזות"
simple_title:         "חיבור מחרוזות"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## למה

לשתף מחרוזות בפייתון הוא כלי חשוב בכתיבת קוד ויכול לשמש להצגת מידע למשתמשים או ליצירת תוכניות מעניינות.

## כיצד

כדי לשתף מחרוזות בפייתון, ניתן להשתמש בפונקציית `+` כדי לחבר מחרוזות באופן אוטומטי, כמו בדוגמאות הבאות:

```Python
message = "שלום"
name = "עולם"
greeting = message + name
print(greeting)

# המסך ידפיס:
# שלוםעולם
```

לחילופין, ניתן להשתמש בפונקציית `format` כדי להכניס ערכים שונים תוך שימוש בסמלי `{}` לתוך מחרוזת, כמו בדוגמאות הבאות:

```Python
name = "דני"
age = 25
intro = "היי, שמי הוא {} ואני בן {}"
print(intro.format(name, age))

# המסך ידפיס:
# היי, שמי הוא דני ואני בן 25
```

## צלילה מלאה

בנוסף, ניתן לעשות תרגילים מעניינים עם שורות הקוד שלנו כדי לשתף מחרוזות בצורה ייחודית. לדוגמה, נוכל להשתמש בלולאת `for` כדי לחבר את כל אותיות המחרוזת לדמותות מעניינות:

```Python
word = "פייתון"
for letter in word:
    print(letter * 2, end="")

# המסך ידפיס:
# פפייייתתוןןן
```

בנוסף, ניתן להשתמש בפונקציות נוספות כמו `join` על מנת לחבר מחרוזות בצורה חכמה יותר וליצור מבני מחרוזות ייחודיים.

## ראה גם

- [מדריך ללולאות for וwhile בפייתון](https://www.python.org/dev/peps/pep-0202/)
- [התיעוד הרשמי של פייתון על מחרוזות](https://docs.python.org/3/tutorial/introduction.html#strings)