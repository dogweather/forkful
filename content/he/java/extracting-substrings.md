---
title:                "Java: חילוץ תת מחרוזות"
simple_title:         "חילוץ תת מחרוזות"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## למה

עיבוד מחרוזות הוא חלק חשוב מתכנות בשפת ג'אווה, ולכן יכול להיות מעניין ללמוד כיצד להבין ולהשתמש בתהליך ההפרדה של מחרוזות לתת-מחרוזות נמצאות בתוךן.

## איך לעשות זאת

בג'אווה, ניתן להשתמש בפונקציית substring כדי להפריד תת-מחרוזות מתוך מחרוזת גדולה. כדי לעשות זאת, יש להעביר את המיקום ההתחלתי והמיקום הסופי של התת-מחרוזת שרוצים להפריד כפרמטרים לפונקציה. כאן יש להיזהר כי המיקומים מתחשבים באינדקס של המחרוזת, כך שהאינדקס הראשון הוא 0.

```Java
String str = "זהו מחרוזת דוגמה";
String subStr = str.substring(3, 9);
System.out.println(subStr);
// תוצאה: חרוז
```

## להעמיק

למידה נוספת על פונקציית substring יכולה לסייע בהבנת האינדקסים והפרמטרים של הפונקציה. כמו כן, כדאי להתנסות עם פרמטרים שונים ולראות את התוצאה שלהם.

## ראיה נוספת

אם ברצונך לדעת עוד על עיבוד מחרוזות בג'אווה, בהמשך תוכל למצוא רשימת מאמרים נוספים העוסקים בנושא הזה:

- [עיבוד מחרוזות בג'אווה](https://he.wikipedia.org/wiki/עיבוד_מחרוזות_בג'אווה)
- [פונקציית substring בג'אווה מדריך](https://www.w3schools.com/java/ref_string_substring.asp)
- [הפרמטרים של פונקציית substring בג'אווה](https://www.baeldung.com/java-string-substring)