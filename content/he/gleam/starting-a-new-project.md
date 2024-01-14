---
title:    "Gleam: התחלת פרויקט חדש"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## למה

קיימים כמה סיבות נפוצות להתחיל פרויקט חדש ב־Gleam. הראשון הוא חזרה לטבע הפשטני של השפה. Gleam מציעה תחביר נוח ופשוט, אשר נועד לאפשר לכתבי הקוד ליצור תכניות נקיות וקלות לתחזוקה. עוד על הפשטנות הזו תיתכן להיות חוויה מעניינת ללמוד על כתיבת קוד נקי וקריא יותר.

## איך לעשות זאת

כדי להמחיש את השימוש ב־Gleam נראה כאן דוגמה פשוטה של קוד, מאתחל רשימה של מספרים שלמים ומדפיס את הממוצע שלהם:

```Gleam
fn average(list) {
    let sum = list
        |> List.foldl(0, &a -> &b -> a + b)
    sum / length(list)
}

let numbers = [3, 6, 9, 12]

IO.print("The average is: ")
IO.inspect(average(numbers))
```

הפלט של הקוד הזה הוא:

```
The average is: 7.5
```

## צלילה עמוקה

כאשר מתחילים פרויקט חדש ב־Gleam ישנם כמה דברים שכדאי לקחת בחשבון. למשל, כתיבת ספריות רייטרי ופשוטות ביותר על מנת לאפשר בניית קוד קריא וקוד נקי. עוד דבר חשוב הוא להבטיח את תדירות ההקוד ולאחזרו לנקודת מוצא כדי לשחזר אותו ככל האפשר לקוד נגיש והגנתי.

## ראה גם

- [מדריך ללמידת Gleam](https://gleam.run/getting-started/)
- [תיעוד רשמי של Gleam](https://gleam.run/documentation/)
- [פרויקטי קוד פתוח נוספים ב־Gleam](https://github.com/search?q=language%3Agleam&type=Repositories)