---
title:                "Haskell: הדפסת פלט מנקודת תיקון שגיאות"
simple_title:         "הדפסת פלט מנקודת תיקון שגיאות"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## למה

הדפסת פלט לאיתור באגים היא כלי חשוב לחולייתי Haskell. היא מאפשרת למתכנתים לבקש ולקבל מידע ממערכת הקוד בזמן הרצה, מאפשרת להבין עקביות בפעולה של התוכנית ומסייעת באיתור ותיקונו של באגים.

## איך לעשות זאת

### אמצעי הדפסה

להדפסת פלט בשפת Haskell ישנם מספר אפשרויות. אחת האפשרויות היא תפקיד הפונקציה print. הפונקציה זו מקבלת פרמטר אחד ומדפיסה אותו למסך. לדוגמה, ניתן לכתוב:

```haskell
print "Hello World!"
```

ולקבל פלט הבא:

```haskell
"Hello World!"
```

### משתנים בלתי מוגדרים

השתמשו בפונקציה print בשביל להציג מידע על משתנים בלתי מוגדרים. לדוגמה, נניח שיש לנו שני משתנים, num1 ו-num2.

```haskell
print num1
print num2
```

תוצאת הפלט כאן יהיה מידע על המשתנים הללו בזמן הרצה, כולל ערכים שניתן לערוך ישירות מתוך הקוד.

## צלילה עמוקה

כעת שיצרנו יכולת להדפיס נתונים, ניתן להעביר כמה מאפיינים נוספים על כלי הדפסה זה. למשל, ניתן להבין כיצד להשתמש באדעות conferences על מנת ליצור פלט תותב מבנה אישי.

## ראו גם

- [Haskell Wiki](https://wiki.haskell.org/Printing_Debug_Output)
- [Debugging in Haskell](https://www.fpcomplete.com/blog/2019/02/debugging-haskell)