---
title:                "המרת תאריך למחרוזת"
html_title:           "Elixir: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

מה זה ולמה?
המרת תאריך למחרוזת הוא תהליך שבו מתבצעת המרה של תאריך מבנה נתונים לסדרת תווים המייצגת את התאריך. תהליך זה חשוב במיוחד לפיתוחנים כדי ליצור תאריך נוח וקריא שיכול לשמש לתצוגה או לפירוק.

איך לעשות:
כדי להמיר תאריך למחרוזת ב-Elixir, ניתן להשתמש בפונקציה ```to_string()``` ולהציב בה את התאריך שצריך להמיר. למשל, אם נציג תאריך כשני פרמטרים לפונקציה ```to_string()```, כך נקבל את התאריך שהומר למחרוזת.
``` Elixir
to_string(Date.new(2019, 10, 25)) #=> "25/10/2019"
```
עוד דוגמא נפוצה להמרת תאריך למחרוזת היא על ידי שימוש בפונקציה ```Calendar. format!/2``` והצבת הנתונים הנכונים בתבנית החלטתית, כמו שנראה בדוגמא הבאה:
``` Elixir
Calendar.format!(~D[2019-10-25], "{M}/{0}/{1}") #=> "10/25/2019"
```

עומק עמוק:
ההמרה של תאריך למחרוזת היא תהליך שנוצר בשנת 1970 עם התקן הפופולרי ISO 8601 עבור תאריכים. למרבה המזל, בן Elan Misto פיתח את ליבת התאריך המובנה ב- Elixir, תוך התבסס על התקן זה. ישנם גם עיבודים אחרים כמו ``Calendar`` שיכולים להמיר תאריכים למחרוזות מהר יותר, אך אין להם כתיבת תאריך קריאה כמו התאריך המובנה ב-Elixir.

ראה גם:
למידע נוסף על הדרך להמיר תאריכים למחרוזות ב-Elixir, ניתן לקרוא בדרייסל הבאים:
- https://hexdocs.pm/elixir/DateTime.html
- https://elixirschool.com/lessons/specifics/date-times/
- https://atomgregor.com/blog/date-time-in-elixir/
- https://devblog.avdi.org/2011/12/28/a-dozen-ways-to-parse-a-date-with-elixir/