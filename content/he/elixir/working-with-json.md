---
title:                "עבודה עם JSON"
date:                  2024-01-19
html_title:           "Arduino: עבודה עם JSON"
simple_title:         "עבודה עם JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON, זה פורמט נתונים פשוט. תוכניתנים משתמשים בזה להחלפת מידע ושמירת תצורה.

## How to:
כדי לעבד JSON ב-Elixir, אפשר להשתמש בחבילה כמו Jason.

התקנה:
```elixir
defp deps do
  [{:jason, "~> 1.2"}]
end
```

קריאה:
```elixir
json_string = "{\"key\": \"value\"}"
{:ok, data} = Jason.decode(json_string)
IO.inspect(data) # מדפיס {"key" => "value"}
```

כתיבה:
```elixir
data = %{"key" => "value"}
json_string = Jason.encode!(data)
IO.puts(json_string) # מדפיס {"key":"value"}
```

## Deep Dive
JSON (JavaScript Object Notation) נבנה על אוטונומיות ג'אווהסקריפט ב-2001. חלופות כוללות XML, YAML. Jason מיימש הכלת נתוני JSON ב-Elixir, יעיל ומאוד קל.

## See Also
- [Jason GitHub Repo](https://github.com/michalmuskala/jason)
- [JSON Specification (RFC 7159)](https://tools.ietf.org/html/rfc7159)
- [Hex.pm (Jason)](https://hex.pm/packages/jason)
