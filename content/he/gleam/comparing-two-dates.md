---
title:    "Gleam: השוואת שתי תאריכים"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# למה
מדוע שימוש בהשוואת שתי תאריכים יכול להיות מועיל בתכנות בשפת Gleam. 

## איך לעשות זאת
להלן דוגמאות קוד ופלט שלהם בתוך קטעי קוד "```Gleam ... ```" 

קטע קוד 1:
```
Gleam -Date.compare(
    Date.from_gregorian_calendar(2021, 10, 20), 
    Date.from_gregorian_calendar(2021, 10, 15)
)
```
פלט 1: `greater`

קטע קוד 2:
```
Gleam -Date.compare(
    Date.from_gregorian_calendar(2019, 5, 10), 
    Date.from_gregorian_calendar(2020, 5, 10)
)
```
פלט 2: `less`

קטע קוד 3:
```
Gleam -Date.compare(
    Date.from_gregorian_calendar(2021, 2, 14), 
    Date.from_gregorian_calendar(2021, 2, 14)
)
```
פלט 3: `equal`

## Deep Dive
השוואת שתי תאריכים נותנת לנו את האפשרות לבצע השוואות לפי תאריכים מדויקים ולבצע פעולות בהתאם לתוצאה. למשל, ניתן לבדוק אם תאריך נתון מופיע לפני או אחרי תאריך אחר, לבדוק אם תאריך נתון נמצא באותו חודש או שנה כמו תאריך אחר, וכו'. כמו כן, ניתן להשתמש בפונקצית השוואה גם כדי למיין תאריכים בסדר כרונולוגי.

כדי להשוות שני תאריכים בשפת Gleam, ניתן להשתמש בפונקציה `Gleam -Date.compare(date1, date2)` כאשר `date1` ו`date2` הם תאריכים בפורמט הספציפי של השפה. הפונקציה תחזיר את אחת מהערכים הבאים: `greater` אם `date1` מגדיל מ`date2`, `less` אם `date1` קטן מ`date2` ו `equal` אם שני התאריכים שווים זה לזה.

## ראה גם
* [מדריך לשפת Gleam](https://gleam.run/get-started/)
* [תיעוד לפונקציות תאריך בשפת Gleam](https://gleam.run/documentation/)
* [פורום לדיונים ועזרה בקהילת Gleam](https://forum.gleam.run/)