---
title:                "Elixir: המרת תאריך למחרוזת"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# למה

בעולם התכנות, לעתים קרובות נדרשת המרת תאריך למחרוזת כדי להציגו למשתמשים בצורה נוחה. בעזרת Elixir, ניתן לבצע פעולה זו בקלות ובפשטות. כתבו יחד איתי ונגלה כיצד לבצע המרה זו במהירות וביעילות.

# איך לבצע את המרה

```elixir
date = {2021, 11, 24}
DateTime.from_tuple(date) |> DateTime.to_string()
```

תוכלו לראות בקוד המופיע לעיל דוגמא לכיצד להמיר תאריך מסויים למחרוזת בעזרת הפונקציות DateTime.from_tuple ו-DateTime.to_string. במקרה שלנו, אנו מעבירים את התאריך {2021, 11, 24} בצורה של tuple לפונקציה DateTime.from_tuple כדי ליצור אובייקט DateTime מתאריך זה, ואז משתמשים בפונקציה DateTime.to_string כדי להמיר אותו למחרוזת בפורמט מועד של יום, חודש ושנה.

לעתים קרובות, ייתכן שתרצו גם להוסיף פורמט אחר למחרוזת כדי להציג את התאריך בצורה אחרת. כדי לעשות זאת, ניתן להשתמש בפונקציות הבנויות DateTime.strftime או Calendar.strftime במקום בפונקציה DateTime.to_string.

```elixir
# DateTime.strftime
DateTime.from_tuple(date) |> DateTime.strftime("%d/%m/%Y")
# פלט: 24/11/2021

# Calendar.strftime
date = [year: 2021, month: 11, day: 24]
date |> Calendar.strftime("%b %d, %Y")
# פלט: Nov 24, 2021
```

ניתן לראות שכדי להשתמש בפונקציה DateTime.strftime, נעביר לה את התבנית המיוחדת של הפורמט שרוצים שהמחרוזת תציג, בעוד שבפונקציה Calendar.strftime אנו נעביר לה את הערכים ישירות כמפתחות בצורת tuple. בשני המקרים, הפלט יהיה מחרוזת בעלת הפורמט הרצוי.

# הצצה לעומק

בעצם, הפונקציות DateTime.from_tuple ו-DateTime.strftime הן פונקציות עזר שמעבירות את