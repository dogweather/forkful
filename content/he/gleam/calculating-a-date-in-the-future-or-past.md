---
title:    "Gleam: חישוב תאריך בעתיד או בעבר"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## למה

יתירת פעמים אנו נדרשים לחשב תאריך בעתיד או בעבר עבור צרכים שונים, כגון תכנון אירוע או ניהול לווים. בפוסט זה נלמד כיצד לבצע חישוב תאריך בעתיד או בעבר באמצעות שפת התכנות Gleam.

## איך לעשות

נתחיל על ידי יצירת קובץ חדש בשם "calculate_date.gleam". נוסיף את הקוד הבא כדי לייבא את הספריות הנחוצות ולהגדיר פונקציה לחישוב תאריך:

```Gleam
use gleam/calendar

pub fn calculate_date(year: Int, month: Int, day: Int, days_to_add: Int) {
    let current_date = Date(year, month, day)
    let future_date = Date.add_days(current_date, days_to_add)

    // נדפיס את התאריך החדש בפורמט של "dd/mm/yyyy"
    let formatted_date = Date.format(future_date, "%d/%m/%Y")

    // נחזיר את התאריך בפורמט טקסטואלי
    formatted_date
}
```

בקוד זה, אנו משתמשים בספריית הלועזית Gleam לייצוג תאריכים ולחישוב תאריכים בעתיד או בעבר. בכדי לחשב את התאריך, אנו מזינים את השנה, החודש, היום ומספר הימים שנרצה להוסיף או להחסיר. לאחר מכן, אנו משתמשים בפונקציית "add_days" כדי לחשב את התאריך החדש ובפונקציית "format" כדי להציג אותו בפורמט של "dd/mm/yyyy". בסופו של דבר, אנו מחזירים את התאריך בפורמט טקסטואלי.

נוסיף עוד קטע קוד כדי לבדוק את הפונקציה שיצרנו:

```Gleam
pub fn main() {
    let future_date = calculate_date(2021, 9, 1, 10)
    IO.format("התאריך החדש הוא: {}", [future_date])
}
```

נוסיף את הפונקציה הבאה כדי לחשב תאריך בעבר:

```Gleam
pub fn calculate_date_in_past(year: Int, month: Int, day: Int, days_to_subtract: Int) {
    let current_date = Date(year, month, day)
    let past_date = Date.sub_days(current_date, days_to_subtract)

    // נדפיס את התאריך החדש בפורמט ש