---
title:                "Javascript: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

##למה

למה ישנם ימים שבהם אנו רוצים לחשב תאריך מסוים בעתיד או בעבר? כי פעמים רבות אנו צריכים לייעץ עם התאריך המדוייק כדי לתכנן את החיים שלנו ולקבוע את המועדים החשובים עבורנו. בעזרת תכנות ב-Javascript, אנו יכולים לכתוב קוד שיעזור לנו לחשב תאריך בעתיד או בעבר.

##כיצד

תחילה, עלינו לציין את התאריך הבסיסי של תאריך המבוקש בעתיד או בעבר. לדוגמה, אם אנחנו רוצים לחשב את התאריך של אחרי 100 ימים מהתאריך הנוכחי, נתחיל עם התאריך הנוכחי ונוסיף אליו 100 ימים. בקוד הבא, אנחנו משתמשים בפונקציית Date של Javascript כדי לייצר את התאריך הנוכחי:

```Javascript
let today = new Date();
```

לאחר מכן, עלינו להוסיף את מספר הימים הנדרשים לתאריך המבוקש. במקרה שלנו, נרצה לחשב את התאריך של אחרי 100 ימים, לכן נוסיף 100 לתאריך הנוכחי:

```Javascript
let futureDate = today.getDate() + 100;
```

כעת, אנו יכולים להציג את התאריך המבוקש באמצעות פונקציות נוספות של Date, כגון פונקציית getMonth ופונקציית getFullYear. הקוד המלא מאחד את כל הפונקציות ומציג את התאריך המבוקש בפורמט ידידותי:

```Javascript
let futureDate = today.getDate() + 100;
let futureMonth = today.getMonth();
let futureYear = today.getFullYear();

console.log("התאריך של אחרי 100 ימים הוא " + futureMonth + "/" + futureDate + "/" + futureYear);
```

הפלט של הקוד הזה יהיה:

`התאריך של אחרי 100 ימים הוא 5/20/2021`

##להעמיק

בנוסף לחישוב תאריך בעתיד, ניתן גם לחשב תאריך בעבר באותו הדרך. כל מה שצריך לעשות הוא להוסיף את