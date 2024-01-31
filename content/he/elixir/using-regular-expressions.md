---
title:                "שימוש בביטויים רגולריים"
date:                  2024-01-19
html_title:           "Bash: שימוש בביטויים רגולריים"
simple_title:         "שימוש בביטויים רגולריים"

category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
מה זה ביטויים רגולריים? זהו שפת תיאור לחיפוש ועיבוד טקסטים בצורה נוחה וגמישה. למה מתכנתים משתמשים בזה? כדי לאתר תווים או מחרוזות בתוך טקסט גדול, לבצע החלפות או לבדוק התאמות עם יעילות.

## How to:
```Elixir
# מציאת מחרוזת
regex = ~r/hello/
"hello world" =~ regex
# פלט: true

# פיצול טקסט לפי רווחים
String.split("שלום עולם", ~r/ /)
# פלט: ["שלום", "עולם"]

# החלפת תבנית במחרוזת
String.replace("דוד מלך ישראל", ~r/דוד/, "שלמה")
# פלט: "שלמה מלך ישראל"
```

## Deep Dive
ביטויים רגולריים התפתחו בשנות ה-60 והם חלק אינטגרלי בעיבוד טקסטים. ישנם חלופות כגון חיפושים סטטיים או שימוש ב-parserים, אך ביטויים רגולריים מציעים גמישות מרבית. ב-Elixir, המימוש מתבצע בעזרת המודול Regex, שמתבסס על הסיפרייה הסטנדרטית של Erlang.

## See Also
- [Elixir Regex documentation](https://hexdocs.pm/elixir/Regex.html)
- [Regex101 for testing expressions](https://regex101.com/)
- [RegexOne for learning regex with interactive exercises](https://regexone.com/)
