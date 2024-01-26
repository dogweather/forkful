---
title:                "עבודה עם YAML"
html_title:           "Bash: עבודה עם YAML"
simple_title:         "עבודה עם YAML"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## מה ולמה? (What & Why?)
YAML הוא פורמט קל לקריאה למידע והגדרות. מתכנתים משתמשים בו להגדרת תצורה, שיתוף נתונים בין שפות תכנות, ועוד, בזכות פשטותו ונוחות בשימוש.

## איך לעשות זאת (How to):
באליקסיר, אין תמיכה טבעית ב-YAML, אך ניתן להשתמש בחבילות כמו `yamerl`. התקנה דרך mix.exs:

```elixir
def deps do
  [
    {:yamerl, "~> 0.8.0"}
  ]
end
```

קריאה ופרסור של YAML:

```elixir
yaml_content = """
---
name: John Doe
age: 30
languages:
  - Elixir
  - Ruby
"""

{:ok, yamerl_constr} = :yamerl_constr.string(yaml_content)
parsed_content = yamerl_constr.documents
IO.inspect(parsed_content)
```

פלט לדוגמא:

```elixir
[
  %{
    "age" => 30,
    "languages" => ["Elixir", "Ruby"],
    "name" => "John Doe"
  }
]
```

## צלילה עמוקה (Deep Dive):
YAML, שזהו ראשי תיבות של "YAML Ain't Markup Language", נוצר ב-2001 כאלטרנטיבה אנושית-קריאה ל-XML. התמיכה ב-YAML באליקסיר לא מובנית בשל הצורך בפשטות ותמציתיות. אולם, עם חבילות כמו `yamerl`, אפשר לטעון ולפרסר נתונים בקלות. חשוב לזכור ש-YAML יכול להיות רגיש לשגיאות כתיב (כמו רווחים עודפים), לכן תמיד יש לבדוק אותו.

## ראו גם (See Also):
- תיקונים: [YAML Lint](http://www.yamllint.com/) לבדיקת תקינות של קוד YAML.
- הסבר מעמיק על YAML ב- [Learn X in Y minutes](https://learnxinyminutes.com/docs/yaml/).
- מדריך רשמי לחבילת `yamerl`: [yamerl documentation](https://hexdocs.pm/yamerl).
- נושאים נוספים ב-Elixir: [Elixir School](https://elixirschool.com/en/).
