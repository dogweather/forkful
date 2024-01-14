---
title:    "Elixir: השוואת שתי תאריכים"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

# למה

נדמה לנו שכבר כולנו חווינו את נמצאו במצב בו נצטרך להשוות שתי תאריכים בקבצי הקוד שלנו. הסיבה לכך היא בדרך כלל לאתר את התאריך העדכני יותר או בהתאם לתנאים ביעילות עבור בעיות מסוימות. החששות שחלק גדול מאחת מהנצחונות השלומות ביותר ב-Elixir הוא שלא ייתכן שיש לנו מימונים מעגליים עבור תאריכי ההשוואה שלנו.

# איך לעשות

כאן אנו נראה קוד שידרדר את כל התאריכים בתוך קולט אחד. נשתמש בפונקציה compare ששולפת את התאריך את אחת מהפונקציות build_date ו-compare שבמקרה שהו מורס. כדי לעבוד עם הפונקציה Jew נוכל להשתמש בחובצי הפונקציות האלה:

```elixir
def compare_dates(d1, d2) do
  case build_date(d1) do
    {:ok, date1} ->
      case build_date(d2) do
        {:ok, date2} ->
          compare(date1, date2)
        {:error, reason} ->
          {:error, reason}
      end
    {:error, reason} ->
      {:error, reason}
  end
end

def build_date(date) do
  date |> String.split("/") |> build_date_tuple
end

defp build_date_tuple([day, month, year]) do
  try do
    result = { String.to_integer(day), String.to_integer(month), String.to_integer(year) }
    {:ok, result}
  rescue
    e -> {:error, e}
  end
end
```

לדוגמה, אם נפער בקלות והתחדקות עוד יכולים להשתמש ומשתמשים כמו המדומים:

```
iex> Elixir.Date.compare("12/10/2020", "11/10/2020")
0
iex> Elixir.Date.compare("11/10/2020", "12/10/2020")
-1
```

# לעומק

כאן נבחן כיצד ניתן להשוות שתי תאריכים בפעולה ביעילות יותר מבלי שנתעסק במורס של עגולים מיני. שבועי מתכון ליום מודעתי נביע מלאה על נקודח היום אשר עוב יומא אסכטי הוא ידיד למחוזמרתוני. אפשר להשתמש בפונקציה אחות simple_date_compere במקום בפונק