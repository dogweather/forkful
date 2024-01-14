---
title:                "Gleam: השוואת שתי תאריכים"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

# למה

למה לאדם להשתתף בהשוואת שתי תאריכים? השוואת תאריכים היא חלק חשוב מתהליך התכנות, מאפשרת לנו לבדוק אם שני אירועים התרחשו באותו זמן או אם האחד התרחש לפני השני. זה יכול להיות מאוד שימושי כאשר אנו עובדים עם תאריכים וזמנים בבניית אפליקציות או אתרים.

# כיצד לבצע

הכיוון הראשון בהשוואת שני תאריכים הוא להשתמש בפונקציה `gleam/datetime` כדי ליצור תאריך חדש מתאריך קיים. ניתן לעשות זאת באמצעות המזהה המותאם להליך המסחרי של גלים `__builder/datetime`. בעזרת הפונקציה הזו, ניתן להשוות שתי תאריכים באמצעות הפונקציה `equal?` או `compare?` ולקבל תשובה בפורמט `gleam/datetime.CompareResult`.

```Gleam
import gleam/datetime
let jan1st = datetime.Builder.datetime(2020, gleam/datetime.January, 1)
let dec25th = datetime.Builder.datetime(2019, gleam/datetime.December, 25)

// השוואת תאריכים
let equal = jan1st |> datetime.equal?(dec25th)
let comparison = jan1st |> datetime.compare?(dec25th)

// פלט
equal == false
comparison == gleam/datetime.CompareResult.Greater
```

# חפירה עמוקה

השוואת שני תאריכים נותנת לנו מידע על דברים רבים נוספים מעבר למראה התאריכים הפשוט. למשל, אנו יכולים להשתמש בפונקציה `diff?` כדי למצוא את ההפרש המדויק בין שני תאריכים, או ניתן להשתמש בפונקציה `add?` כדי להוסיף או להחסיר ימים, שעות או דקות מתאריך נתון.

אם אנו רוצים לבדוק אם תאריך מסוים נופל ברחוב ודוקא ביום חג או יום שנה רביעי, ניתן להשתמש בפונקציה `isleapyear?` או `easter_sunday?`.

דבר אחרון שאנו יכולים לעש