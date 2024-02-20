---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:04.199650-07:00
description: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05E7\u05D1\u05E6\u05D9\
  \ CSV (\u05E2\u05E8\u05DB\u05D9\u05DD \u05DE\u05D5\u05E4\u05E8\u05D3\u05D9\u05DD\
  \ \u05D1\u05E4\u05E1\u05D9\u05E7) \u05DB\u05D5\u05DC\u05DC\u05EA \u05E7\u05E8\u05D9\
  \u05D0\u05D4 \u05DE\u05EA\u05D5\u05DA \u05D5\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\
  \u05E7\u05D1\u05E6\u05D9\u05DD \u05D0\u05DC\u05D5, \u05E6\u05D5\u05E8\u05DA \u05E0\
  \u05E4\u05D5\u05E5 \u05DC\u05DE\u05E9\u05D9\u05DE\u05D5\u05EA \u05D4\u05D3\u05D5\
  \u05E8\u05E9\u05D5\u05EA \u05D9\u05D9\u05D1\u05D5\u05D0/\u05D9\u05E6\u05D5\u05D0\
  \ \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D0\u05D5 \u05E4\u05EA\u05E8\u05D5\u05E0\
  \u05D5\u05EA \u05D0\u05D7\u05E1\u05D5\u05DF \u05E4\u05E9\u05D5\u05D8\u05D9\u05DD\
  . \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\u2026"
lastmod: 2024-02-19 22:04:58.088651
model: gpt-4-0125-preview
summary: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05E7\u05D1\u05E6\u05D9 CSV\
  \ (\u05E2\u05E8\u05DB\u05D9\u05DD \u05DE\u05D5\u05E4\u05E8\u05D3\u05D9\u05DD \u05D1\
  \u05E4\u05E1\u05D9\u05E7) \u05DB\u05D5\u05DC\u05DC\u05EA \u05E7\u05E8\u05D9\u05D0\
  \u05D4 \u05DE\u05EA\u05D5\u05DA \u05D5\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E7\
  \u05D1\u05E6\u05D9\u05DD \u05D0\u05DC\u05D5, \u05E6\u05D5\u05E8\u05DA \u05E0\u05E4\
  \u05D5\u05E5 \u05DC\u05DE\u05E9\u05D9\u05DE\u05D5\u05EA \u05D4\u05D3\u05D5\u05E8\
  \u05E9\u05D5\u05EA \u05D9\u05D9\u05D1\u05D5\u05D0/\u05D9\u05E6\u05D5\u05D0 \u05E0\
  \u05EA\u05D5\u05E0\u05D9\u05DD \u05D0\u05D5 \u05E4\u05EA\u05E8\u05D5\u05E0\u05D5\
  \u05EA \u05D0\u05D7\u05E1\u05D5\u05DF \u05E4\u05E9\u05D5\u05D8\u05D9\u05DD. \u05DE\
  \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\u2026"
title: "\u05E2\u05D5\u05D1\u05D3\u05D9\u05DD \u05E2\u05DD CSV"
---

{{< edit_this_page >}}

## מה ולמה?

עבודה עם קבצי CSV (ערכים מופרדים בפסיק) כוללת קריאה מתוך וכתיבה לקבצים אלו, צורך נפוץ למשימות הדורשות ייבוא/יצוא נתונים או פתרונות אחסון פשוטים. מתכנתים מנצלים את היכולת הזו להחלפת נתונים בין מערכות, עריכת נתונים מהירה, או למקרים שבהם פורמט נתונים קליל וקל לשינוי הוא יתרון.

## איך לעשות:

Elixir, עם התאמת תבניות החזקה שלו והתמיכה בפייפליינינג, יכול לטפל בקבצי CSV ביעילות, גם ללא ספריות צד שלישי. עם זאת, לצרכים מתקדמים יותר, הספרייה `nimble_csv` היא בחירה מהירה ופשוטה.

### קריאה של קובץ CSV ללא ספריות חיצוניות

ניתן לקרוא ולפענח קובץ CSV על ידי שימוש בפונקציות הקיימות ב-Elixir:

```elixir
defmodule CSVReader do
  def read_file(file_path) do
    File.stream!(file_path)
    |> Stream.map(&String.trim_trailing/1)
    |> Stream.map(&String.split(&1, ","))
    |> Enum.to_list()
  end
end

# שימוש לדוגמה
CSVReader.read_file("data.csv")
# פלט: [["Header1", "Header2"], ["Row1Value1", "Row1Value2"], ["Row2Value1", "Row2Value2"]]
```

### כתיבה לקובץ CSV

בדומה, לכתיבת נתונים לקובץ CSV:

```elixir
defmodule CSVWriter do
  def write_to_file(file_path, data) do
    File.open(file_path, [:write], fn file ->
      Enum.each(data, fn row ->
        IO.write(file, Enum.join(row, ",") <> "\n")
      end)
    end)
  end
end

# שימוש לדוגמה
data = [["Header1", "Header2"], ["Value1", "Value2"], ["Value3", "Value4"]]
CSVWriter.write_to_file("output.csv", data)
# יוצר output.csv עם הנתונים בפורמט CSV
```

### שימוש ב-`nimble_csv`

לטיפול מורכב יותר ב-CSV, `nimble_csv` מספק דרך חזקה וגמישה לעבוד עם נתוני CSV. ראשית, הוסף את `nimble_csv` לתלות שלך ב-`mix.exs` והרץ `mix deps.get`:

```elixir
defp deps do
  [
    {:nimble_csv, "~> 1.2"}
  ]
end
```

פיענוח נתוני CSV עם `nimble_csv`:

```elixir
defmodule MyCSVParser do
  NimbleCSV.define(MyParser, separator: ",", escape: "\\")

  def parse(file_path) do
    file_path
    |> File.stream!()
    |> MyParser.parse_stream()
    |> Enum.to_list()
  end
end

# שימוש לדוגמה
MyCSVParser.parse("data.csv")
# הפלט עם nimble_csv ניתן להתאמה בהתבסס על ההגדרה, אך בדרך כלל נראה כמו רשימת רשימות או צמדים תלוי באיך שהגדרת את המפענח שלך.
```

כתיבת נתוני CSV באמצעות `nimble_csv` דורשת המרה ידנית של הנתונים שלך לפורמט הנכון ולאחר מכן כתיבתם לקובץ, דומה מאוד לדוגמה הפשטית של Elixir אך מנצלת את `nimble_csv` ליצירת שורות CSV מעוצבות נכון.

בבחירה של הגישה המתאימה למורכבות המשימה שלך, תוכל לטפל בקבצי CSV ב-Elixir עם גמישות ועוצמה רבה.
