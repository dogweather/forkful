---
title:                "Python: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## למה

מתרגילים רבים בתכנות נתקלים במצבים שבהם יש צורך להמיר טקסט לאותיות קטנות. המרת הטקסט לאותיות קטנות חשובה מאוד כאשר עובדים עם טקסט שנמצא בקשר לחיפוש ולמיון.

## כיצד לבצע את המרה?

המרת טקסט לאותיות קטנות בשפת פייתון היא פשוטה ומהירה עם פעולת "lower()". ניתן להשתמש בפעולה כך: 

```python
string = "טקסט עם אותיות גדולות"
lowercase_string = string.lower()

print(lowercase_string)

# output: טקסט עם אותיות גדולות
```

בדוגמה זו, את המשתנה "string" המכיל טקסט עם אותיות גדולות אנו ממירים לטקסט עם אותיות קטנות ושמים אותו במשתנה "lowercase_string". כך, נוכל להשתמש בטקסט המוסתר צורךי כמו לחיפוש ולמיון.

## מעמקים נמרץים

כדי להבין יותר על פעולת "lower()" נוכל לבצע מחקר עמק יותר על כיצד היא מתבצעת בעומק ואיך התוצאה נכתבת על המסך. 

בקצרה, פעולת "lower()" ממירה את כל האותיות הגדולות בטקסט לאותיות קטנות באמצעות שימוש בטבלת יחסי תווים שבה האותיות נמצאות בזוג יחסי. כך, מתקבל תוצאה סופית שבה הטקסט כולו כתוב באותיות קטנות.

## ראה גם

- פעולת "upper()" להמרת טקסט לאותיות גדולות: https://www.w3schools.com/python/ref_string_upper.asp
- פעולת "capitalize()" להמרת האות הראשונה במילה לאות גדולה: https://www.w3schools.com/python/ref_string_capitalize.asp
- פעולת "swapcase()" להמרת אותיות גדולות לקטנות ולהפך: https://www.w3schools.com/python/ref_string