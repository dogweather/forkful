---
title:                "חיפוש והחלפת טקסט"
html_title:           "Python: חיפוש והחלפת טקסט"
simple_title:         "חיפוש והחלפת טקסט"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## למה

זהו פעולה שכולנו נדרשים לבצע במהלך התכנות - לחילופי טקסט. זו אופציה חזקה שאפשרית בשפת פייתון ונותנת לנו את היכולת לשנות חלקים בטקסט שלנו תוך שמירה על פורמט המקורי.

## כיצד לבצע חילופי טקסט עם פייתון

הראה את הקורות התחיליים של הקוד שלכם כדי לאתר את הטקסט שאתם מעוניינים לשנות. לאחר מכן, תוכלו להשתמש בפונקציה "replace" כדי לבצע את השנויים הרצויים. ניתן לראות למטה דוגמאות לשימוש בפונקציה זו ואיך הטקסט נשנה בתוך הקוד.

```Python
# דוגמא לשינוי שם משתמש באתר
old_username = 'old_user'
new_username = old_username.replace('old', 'new')
print(new_username) # יוצא: new_user
```

```Python
# דוגמא לאיפוס סיסמה ובצע סיסמה חדשה בתוך התכנית
old_password = '1234'
new_password = old_password.replace(old_password, '4321')
print(new_password) # יוצא: 4321
```

## חילופי טקסט מעמיקים יותר

בנוסף לפונקציה הפשוטה "replace", פייתון מציעה גם אפשרויות נוספות לחילופי טקסט עשירות יותר. בין התכונות האלו ניתן למצוא פונקציית "find", שמאפשרת לחפש מחרוזת מסוימת בתוך טקסט ולהחליף את המחרוזת שנמצאה במיקום מסוים. כמו כן, ניתן להשתמש בפונקציית "replace" עבור מספרים ולעבוד עם ערכים מספריים במקום מחרוזות.

## ראו גם

- [Documentation for the replace() function in Python](https://docs.python.org/3/library/stdtypes.html#str.replace)
- [Detailed tutorial on using replace() in Python](https://www.pythoncentral.io/pythons-string-replace-method-replacing-python-strings/)