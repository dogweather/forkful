---
title:                "עבודה עם קבצי CSV"
date:                  2024-01-19
html_title:           "Arduino: עבודה עם קבצי CSV"
simple_title:         "עבודה עם קבצי CSV"

category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם קבצי CSV (ערכים מופרדים בפסיקים) נפוצה בתוכנות עבור קריאה וכתיבה של נתונים טבלאיים פשוטים. תוכניתנים משתמשים בזה כי זה תקן תעשייתי, נגיש וקל לעיבוד בכלי גיליונות עבודה כמו Excel.

## איך לעשות:
באליקסיר, עבודה עם CSV יכולה להתבצע בעזרת ספריית חיצונית. דוגמא לקריאה וכתיבה של קובץ CSV:

```elixir
# כדי להשתמש בספריית CSV תצטרך להוסיף את :csv בתלותות של הפרויקט שלך
defmodule CSVExample do
  require CSV

  def read_csv(file_path) do
    file_path
    |> File.stream!()
    |> CSV.decode(headers: true)
    |> Enum.to_list()
  end

  def write_csv(data, file_path) do
    CSV.encode_to_iodata(data, headers: true)
    |> Enum.map(&IO.iodata_to_binary(&1))
    |> :ok = File.write(file_path)
  end
end

# קריאת CSV
data = CSVExample.read_csv("data.csv")
# כתיבת CSV
CSVExample.write_csv(data, "new_data.csv")
```
הדפסת `data` תראה לנו את הנתונים שנקראו מהקובץ.

## ניתוח עמוק
התקן CSV התפתח לאורך השנים וכלול ברוב שפות התכנות. אלטרנטיבות פופולריות כוללות JSON ו-XML, שהם יותר גמישים אך פחות נגישים לצופים רגילים. בביצוע יש לשים לב לסוגי הנתונים ולבעיות של תאימות (לדוג', שדות עם פסיקים בתוכם).

## ראה גם
- מסמך התקנים לCSV: https://tools.ietf.org/html/rfc4180
- הספריה `CSV` ב- Hex: https://hex.pm/packages/csv
- תיעוד Elixir לטיפול בקבצים: https://hexdocs.pm/elixir/File.html
