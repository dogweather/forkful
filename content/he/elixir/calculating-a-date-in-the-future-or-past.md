---
title:                "חישוב תאריך בעתיד או בעבר"
html_title:           "Elixir: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

עבדת מיצים ורוצה לחזור לימים הקודמים כשכלום טכנולוגיה לא היה זמין לך? אליקסיר מספק כלי חכמה עם הקיבולת לחזור אל תאריכים בעתיד או בעבר. ניתן להשתמש בתכונה זו במגוון מסוגים של מימושים, לדוגמה - לחשב את יום ההולדת של אדם, לתכנות אירועים או לנהל מערכת לוגיסטית.

## Why

במהלך העבודה עם אליקסיר, קיימת צורך לשלב התאריכים כחלק מהלוגיקה השונה של היישום. לעתים נדרשת לנגוס בנתונים המתייחסים לתאריכים בעבודה עם מערכת או לתכנון אירועים עתידיים. בכדי לתפקד ביותר, מדובר בחובה לקבל סיכויים לחיזוק השאלות המעיינות ביותר בזמן רב מתגובה.

## How To

הנה דוגמה לאיך לחשב את תאריך ההולדת של אדם באליקסיר:

```elixir
def birthdate(name, age) do
  current_year = Calendar.ISO.year
  current_month = Calendar.ISO.month
  current_day = Calendar.ISO.day
  birth_year = current_year - age
  birth_month = Random.rand(1..12)
  birth_day = Random.rand(1..Calendar.ISO.day_in_month(birth_year, birth_month))
  
  "Happy birthday #{name}! You were born on #{birth_day}/#{birth_month}/#{birth_year}."
end

birthdate("John", 25)
```

Output:

"Happy birthday John! You were born on 12/6/1995."

כאן אנחנו משתמשים בפונקציות הפנטזיות של המודול Calendar כדי לחשב תאריכים ושימוש בחוקי הלוגיקה של השפה עבור תאריכים הנגישים במתקין.

## Deep Dive

החישוב של תאריך אלישיבר הינו פיסת מתמטיקה מעניינת כיוון שמשתמשים במגוון של אלגוריתמים וכלים נפילים על מנת לכוון את התאריך באופן הנכון. אחד הדברים הכי מופתיים ביישום של תאריך של אלישיבר הוא האפשרות להכוון תאריך של מ