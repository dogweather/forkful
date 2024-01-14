---
title:    "Elixir: חיפוש והחלפת טקסט."
keywords: ["Elixir"]
---

{{< edit_this_page >}}

# מדוע

אנשים עשויים להשתמש בחיפוש והחלפת טקסט כדי להשתמש ברעיונות כמו כתיבת קוד ועריכת מחרוזות.

# כיצד לעשות

```Elixir
# הנה דוגמה של החלפת טקסט באמצעות הפונקציה `String.replace`
string = "שלום עולם"
String.replace(string, "עולם", "מדינה")

# Output: "שלום מדינה"
```

```Elixir
# ניתן גם להשתמש בביטויים רגולריים כדי לחלוף על מחרוזות מתאימות
string = "Is it spelled gray or grey?"
Regex.replace(~r/grey/, string, "gray")

# Output: "Is it spelled gray or gray?"
```

# חפירה עמוקה

חיפוש והחלפת טקסט הוא כלי חשוב לניהול טקסטים ולעיצוב קוד בדרך הכי יעילה ומהירה. בכדי ללמוד עוד על כיצד להשתמש בכלי זה במיטבה, תוכלו לעיין במדריך המפורט של Elixir על חיפוש והחלפת טקסט.

# ראה גם

- [Elixir נמצא באמצע קו המרה](https://elixir-lang.org/getting-started/introduction.html)
- [מדריך נוסף על חיפוש והחלפת טקסט ב- Elixir](https://hexdocs.pm/elixir/String.html#replace/4)