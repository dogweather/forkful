---
title:                "Elixir: שרשור תווים"
simple_title:         "שרשור תווים"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

# למה
הכתיבה בנקל היא חלק חשוב מתהליך התכנות. חיבור מחרוזות הוא כלי חיוני ליצירת משתנים מורכבים ולתצורה של מחרוזות מותאמות לצרכים מסוימים.

# כיצד להכין
שתי מחרוזות ניתנות לחיבור על ידי שימוש בסימן ′+′. בעזרת יכולת הטיפול הוגנת של שפת Elixir בסוגי נתונים שונים, ניתן לחבר בנקל מחרוזות לספרתים, מערכים ועוד.

```Elixir
"שלום" + " " + "עולם" #=> "שלום עולם"
"ספרת" + 5 |> Kernel.toString #=> "ספרת5"
[72, 69, 76, 76, 79] |> Enum.map(&Integer.to_string/1) |> Enum.join("") #=> "HELLO"
```

# עיון מעמיק
בשפת Elixir, מחרוזת היא בעצם רשימת תווים. כאשר אנו מחברים שתי מחרוזות, אנו יוצרים בעצם רשימה חדשה אשר מכילה את כל התווים משתי המחרוזות המקוריות. במקרה של חיבור מחרוזת עם סוגי נתונים אחרים, השפה ישתמש בפונקציות פנים-מובנות כדי להמיר אותם למחרוזת.

# ראה גם
- פונקציות מובנות לאחר חיבור מחרוזות בגיאווארט: https://hexdocs.pm/elixir/String.html#concatenate/1
- מקורות ליצירת מחרוזות מותאמות אישית באמצעות פונקציות StringBuilder: https://dev.to/surfing-elixir/custom-string-builder-with-elixir-10jg