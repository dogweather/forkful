---
title:                "חיפוש והחלפת טקסט"
html_title:           "Elixir: חיפוש והחלפת טקסט"
simple_title:         "חיפוש והחלפת טקסט"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## מה זה & למה?
חיפוש והחלפת טקסט הוא תהליך בו מציאת טקסט ספציפי בתוך קובץ והחלפתו בטקסט אחר. תהליך זה נחשב לחשוב וקשה למתכנתים, מכיוון שהם צריכים להיות מדויקים בהתאמה של הטקסט ולוודא שאין שינויים לא צפויים.

## איך לעשות:
```Elixir
defmodule StringManipulator do
  def search_replace(string, search_term, replace_term) do
    String.replace(string, search_term, replace_term)
  end
end

StringManipulator.search_replace("Hello World", "World", "Universe") #=> "Hello Universe"
```
קוד זה משתמש בפונקציה מובנית עבור חיפוש והחלפת טקסט, String.replace, כדי להחליף מחרוזת עם מחרוזת אחרת. הפונקציה זו מקבלת שלושה ארגומנטים: המחרוזת המקורית, מחרוזת החיפוש ומחרוזת ההחלפה. בדוגמה זו, המחרוזת "Hello World" נחלפת עם המחרוזת "Universe".

## טיפול מקיף:
ישנם אלטרנטיבות לחיפוש והחלפת טקסט כמו לאסוף את הטקסט עם קוד PATTERN שונה או ליצור פונקציות מותאמות אישית שיכולות לחפש ולהחליף טקסט. בנוסף, פונקציית חיפוש והחלפת הטקסט אמורה להיות כמה שיותר ביצועית ככל האפשר כדי לאפשר שימוש בתהליך זה בקבצים גדולים.

## ראו גם:
- [פנאיון על חיפוש והחלפת טקסט ב-Elixir](https://elixir-lang.org/getting-started/pattern-matching.html)
- [שפה שוודית בעלת דומיין ספציפי עבור חיפוש והחלפת טקסט](https://www.vim.org/)
- [אפשרויות טיפול במחרוזת בנתיב Nandwani, כולל פרמטריזציה](https://www.tutorialspoint.com/data_structures_algorithms/string_manipulation.html)