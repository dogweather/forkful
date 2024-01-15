---
title:                "לקבלת התאריך הנוכחי"
html_title:           "Elm: לקבלת התאריך הנוכחי"
simple_title:         "לקבלת התאריך הנוכחי"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## למה

למה בעצם להתעסק עם תאריך נוכחי בתכנות ב-Elm? כי התאריך הנוכחי הוא ממש מועיל לכל מיני מטרות כמו לתאר את התאריך המדוייק של משהו, ליצור תעודת זיהוי או לקבוע את הזמן הנכון ליציאה מספציפית. יש לנו את הבנה שב-Elm בדיוק איך לקבל את התאריך הנוכחי ולעשות זאת בצורה יעילה.

## כיצד

הנה כמה דוגמאות קוד מתעדכנות עם התאריך הנוכחי בתכנות ב-Elm:

```Elm
getDate : Date
getDate =
  Date.now
```

זה יחזיר את התאריך הנוכחי בפורמט מספיק לכל מטרה שנזדקק לה. אם אנחנו רוצים להציג את התאריך בפורמט טוב יותר, ניתן להשתמש בפונקציה `toString` מול התאריך הנוכחי:

```Elm
getDateInString : String
getDateInString =
  Date.now
    |> Date.toString
```

זה יחזיר את התאריך בפורמט נוח. ניתן גם לשנות את הפורמט על ידי שימוש בפרמטרים נוספים לפונקציה `toString`, כל אחד מהם מייצג כדי להפריד בין חלקי התאריך בפונקציה השלישית:

```Elm
getDateFormatted : String
getDateFormatted =
  Date.now
    |> Date.months
    |> Date.days
    |> Date.years
    |> Date.toString ["-", "-", ""]
```

זה יחזיר מחרוזת בפורמט שמכיל חלקי התאריך מופרדים עם מקומות רוודים.

## טיול בים

אם אתה מתעניין לחקור עוד על תאריך נוכחי בתכנות ב-Elm, כדאי להכיר את פונקציות `Date.fromParts` ו`Date.fromString` שמאפשרות יצירת תאריך מכל חלקיו והמרת מחרוזת לתאריך בפורמט שנרצה. כמו כן, תוכל לחקור עוד על פונקציות נוספות כמו `Date.second`, `Date.toIsoString`, ועוד.

##