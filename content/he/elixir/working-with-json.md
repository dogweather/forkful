---
title:                "Elixir: עבודה עם json"
simple_title:         "עבודה עם json"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/working-with-json.md"
---

{{< edit_this_page >}}

# למה:
תוכן זה יועיל לכל מתכנת שרוצה ללמוד איך לעבוד עם נתוני JSON בזהירות במהלך פיתוח בָּאלי־מקבע.

## איך לעבוד עם JSON ב-Elixir
Elixir מספק כמה דרכים לעבוד עם JSON. אחת הדרכים הפשוטות היא להשתמש בפונקציה `Jason.encode!/1` כדי להמיר נתוני Elixir ל-JSON.
```Elixir
data = %{name: "ציפורי", age: 28}
Jason.encode!(data)
```
תוצאה:
```json
{"name": "ציפורי", "age": 28}
```

אם ברצונך להמיר JSON לנתוני Elixir, תוכל להשתמש בפונקציה `Jason.decode!/1` כדי להמיר את מחרוזת ה-JSON למחרוזת של נתוני Elixir. לדוגמה:
```Elixir
json = ~s({"name": "ציפורי", "age": 28})
Jason.decode!(json)
```
תוצאה:
```Elixir
%{name: "ציפורי", age: 28}
```

כדי להתאים לנתוני טיפוס מסוים ב-Elixir, ניתן להשתמש בפונקציות הבאות:
- `Jason.encode!/2` - כדי להמיר את הנתונים לפורמט JSON ולהוסיף מפתחים עבור כל ערך.
- `Jason.encode!(2, keys: :atoms)` - כדי להמיר את הנתונים לפורמט JSON ולהתאים למפתחים של אטומים.
- `Jason.encode!(2, keys: :strings)` - כדי להמיר את הנתונים לפורמט JSON ולהתאים למפתחים של מחרוזות.

## עוזב עמקים
כמו שאתה רואה, עבודה עם JSON פשוטה וקלה ב-Elixir תודות לקוד הפתוח של Jason. תוכל למצוא מידע נוסף על הספרייה ועל כיצד להשתמש בה באתר הרשמי של Jason ב- [GitHub](https://github.com/michalmuskala/jason). כמו כן, אתה יכול לראות דוגמאות נוספות ולהתנפץ בקהילת ה-Elixir על [Discord](https://discord.com/invite/elixir-lang) ו[Stack Overflow](https://stackoverflow.com/questions/tagged/elixir).

# ראה גם:
- [איך ליצור נושאים המייצגים OBJ ב-Elixir, בנספח עבודת ברברה מייזל