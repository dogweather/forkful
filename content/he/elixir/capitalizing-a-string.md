---
title:                "הפיכת מחרוזת לאותיות רישיות"
html_title:           "Bash: הפיכת מחרוזת לאותיות רישיות"
simple_title:         "הפיכת מחרוזת לאותיות רישיות"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
קיבוע אותיות ראשיות היא שיטה שבה האות הראשונה במחרוזת הופכת לאות רישית (גדולה) ושאר האותיות לקטנות. תוכניתנים משתמשים בה לתקן טקסט לתצוגה נקיה וקונסיסטנטית, לדוגמה, שמות אנשים או כותרות.

## איך לעשות:
```elixir
defmodule StringTools do
  def capitalize(string) do
    string
    |> String.downcase()
    |> String.capitalize()
  end
end

IO.puts StringTools.capitalize("שלום עולם") # Outputs: "שלום עולם"
IO.puts StringTools.capitalize("HELLO") # Outputs: "Hello"
```
הערה: באליקסיר אין שינוי עבור אותיות ראשיות בעברית.

## נחפור עמוק יותר
לקיבוע אות ראשית יש תולדות ארוכות בתכנות מחשבים, שמתחילות עם צורך בתקנון ערכים שמכילים טקסט, כמו שמות פרטיים ועסקיים. באליקסיר, `String.capitalize/1` ידאג לזה שהאות הראשונה תהיה רישית ושאר המחרוזת באותיות קטנות. ישנם גם דרכים חלופיות לביצוע המשימה הזו, כמו לשלב פונקציות כמו `String.slice/2` ו-`String.upcase/1`, אבל הפתרונות המובנים הם הבחירה הנקייה והיעילה ביותר. הקפד לבדוק שהמחרוזת לא תתחיל בסימן שאינו אות אם התוצאה חשובה לך.

## ראו גם
- [Elixir String Module Documentation](https://hexdocs.pm/elixir/String.html)
- [Unicode Normalization Forms](https://unicode.org/reports/tr15/)