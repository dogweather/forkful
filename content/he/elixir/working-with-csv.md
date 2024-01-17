---
title:                "עבודה עם קבצי CSV"
html_title:           "Elixir: עבודה עם קבצי CSV"
simple_title:         "עבודה עם קבצי CSV"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## מה ולמה?
טוב, אז CSV זה סוג של קובץ טקסט שמכיל נתונים בפורמט של טבלה, תוכלו לחשוב על זה כמו גליון אלקטרוני שמכיל רשימת נתונים. למה תרצו לעבוד עם CSV? אז כאשר אתם מפעילים תוכניות וצריכים לטפל בנתונים שמכילים מידע ממוקד, כמו רשימת לקוחות או נתוני עסקאות, זה יכול להיות קל יותר לעבוד עם CSV במקום לעבוד ישירות עם קובץ טקסט רגיל.

## איך לעבוד עם זה?
בלי לדבר הרבה, הנה דוגמאות פשוטות של קוד ב-Elixir שמראות איך לפענח נתונים מקובץ CSV ולעבוד עם התוכן שלו:

```Elixir
require CSV

# קרא קובץ CSV
csv_file = "כתובת קובץ של CSV"
file = File.open!(csv_file)
csv = file |> CSV.decode()

# להדפיס את הנוסחא של הכותרות בקובץ
IO.puts Enum.join(csv.headers, ", ")

# לעבור לכל נתוני השורות ולעבוד איתם
Enum.each(csv.rows, fn row ->
  first_name = row["first_name"]
  last_name = row["last_name"]
  age = row["age"]
  IO.puts "#{first_name} #{last_name} is #{age} years old."
end)

```

הפלט בסיום ייראה כך:

```Elixir
first_name, last_name, age
John, Doe, 30
Jane, Smith, 25
Mike, Johnson, 40
```
"ג'ון דו הוא 30 שנים." <br>
"ג'יין סמית' היא 25 שנים." <br>
"מייק ג'ונסון הוא 40 שנים." <br>

כמו שאתם רואים, עם הקוד הנ"ל תוכלו לפענח את כל הנתונים מהקובץ CSV ולבצע איזו תכונה שתרצו עם התוצאות.

## יעולות
אם אתם מחפשים תכנית עם יכולות נרחבות יותר, אתם יכולים לנסות לעבוד עם חבילת CSV אחרת כמו CSVelo או Gabi. כמו כן, כאשר אתם עובדים עם קבצים גדולים, ניתן לשקול למצוא פתרון אחר כמו עבודה עם מאגרי נתונים מקומיים.

## המשך לעמוד
אם אתם מעוניינים ללמוד עוד על עבודה עם CSV ב-Elixir, תוכלו למצוא מידע נוסף ופרטים מעמיקים באתר הרשמי של Elixir או בקבוצת אתרי Github העשירה שמכילה כלים נוספים ודוגמאות תוכניות לעבוד עם קבצי CSV.