---
title:                "Swift: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## מדוע

למה מישהו יעדיף להמיר תאריך למחרוזת בשפת סוויפט?

## איך לעשות זאת

בשפת סוויפט קיימת פונקציה מובנית בשם `string(from:)` שמאפשרת להמיר תאריך למחרוזת. הנה מספר דוגמאות של כיצד ניתן להשתמש בפונקציה זו:

```Swift
let date = Date()
let formatter = DateFormatter()
formatter.dateFormat = "dd/MM/yyyy"
let stringDate = formatter.string(from: date)

// פלט: 06/08/2021
```

אם נרצה להציג את התאריך עם שעה, ניתן לשנות את הפורמט של המחרוזת:

```Swift
formatter.dateFormat = "dd/MM/yyyy HH:mm:ss"

// פלט: 06/08/2021 15:24:40
```

כמו כן, ניתן להשתמש בפונקציה זו עבור תאריך מסוים שנמצא בטווח מסוים. לדוגמה, ניתן להציג רק את היום והחודש של התאריך:

```Swift
formatter.locale = Locale(identifier: "he_IL") // דוגמה של הצגת התאריך בעברית
formatter.dateFormat = "dd MMMM" // ניתן להשתמש גם במילים או סימני פיסוק במחרוזת

let stringDate = formatter.string(from: date)

// פלט: 06 אוגוסט
```

## מעמקים

בסיסית, המרת תאריך למחרוזת נעשית באמצעות פונקציית הפורמט `dateFormat` של `DateFormatter`. ניתן לשנות את התבנית של התאריך על ידי הוספת או הסרת אותיות וסימנים בסדר הרצוי.

כדי להציג את התאריך בכל שפה, ניתן להשתמש בניתוחי שפות כדי לקבוע את משמעות התבנית. כמו כן, ניתן להשתמש בספריית `Foundation` כדי להציג טוענות שפה מותאמות אישית לשפת המחשב.

## ראו גם

* [מדריך: המרת תאריך למחרוזת בפייתון](https://www.digitalocean.com/community/tutorials/how-to-convert-data-to-strings-in-python)
* [תיעוד רשמי על פונקציית `string(from:)` בשפת סוויפט](https://developer.apple.com/documentation/foundation/dateformatter/140