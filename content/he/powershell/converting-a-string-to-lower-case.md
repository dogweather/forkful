---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "PowerShell: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# מה ולמה?

המרה של מחרוזת לאותיות קטנות היא פעולה שמתבצעת על מחרוזת כדי להפוך את כל האותיות שבה ללאותיות קטנות בלבד. פעולה זו נחשבת חשובה על מנת להבטיח שהמידע המוגדר במחרוזות תמיד יהיה אחיד והדרגתי. בכדי לקרוא בקול נמוך יותר ועל מנת להקל על אירועי תרגום.

## איך לעשות זאת?

```PowerShell
$str = "הפנקס הזה רץ לאט"
Write-Host $str.ToLower()
```
```PowerShell
הפנקס הזה רץ לאט
```

```PowerShell
$str = "הפנקס הזה רץ לאט"
Write-Host $str.ToLower().GetType()
```
```PowerShell
System.String
```

## כיוונון עמוק

בתחום התכנות, מרבית שפות התכנות מציעות פעולה להמרת מחרוזת לאותיות קטנות. זה נעשה בכדי להבטיח שכל האותיות במחרוזות יהיו באותו המבנה ושוהים במיקומים מדוייקים בשורות טקסט. בנוסף, תיקון התחלקות במיקרו יכול להקל על אתרים גדלים של כתיבה, פעולות מלאות תאונות כמו דחיסות קבצים או עיבוד נתונים.

## ראה גם

כדי לקבל מידע נוסף על מחרוזות בכלל או ליצור מחרוזת בפייתונים יש לך את המאמר שלנו [כיצד לעבוד עם מחרוזות בפייתונים](https://www.python.org/dev/peps/pep-0498/) ו [התחל בסיכום המטרות שאתה מעוניין להשתמש](https://medium.com/towards-artificial-intelligence/summarization-a-quick-guide-to-text-summarization-796c28d5646e). נשמח לשמוע על חוות משמעותיות לכל מאמר מהסוג הזה שנודע לך!