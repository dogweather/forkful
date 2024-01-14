---
title:    "Elixir: הדפסת פלט איתור שגיאות"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

**
# למה
קוראים ל 1-2 הסיבות על מנת לפרגינג בדוק באליקסר

## כיצד לעשות 
אפשר להדפיס תצוגות בדיקה באליקסר באמצעות `IO.inspect / 2` פונקציות. הנה דוגמאות קוד ופלט:

```elixir
defmodule Person do
  defstruct name: "John", age: 30
end

person = %Person{name: "Jane", age: 25}

# Inspecting a single variable
IO.inspect(person.age)

# Output: 25

# Inspecting multiple variables
IO.inspect(person.name, label: "Name")
IO.inspect(person.age, label: "Age")

# Output:
# Name: "Jane"
# Age: 25
```

## חקירה עמוקה 
הדפסת תצוגות בדיקה יכולה לעזור לנו לבדוק את ערכי המשתנים או התוצאות של פעולות באופן פשוט יותר. ניתן להשתמש בפלט המודפס לנוחיות טיפול בכפלים או בתנאים מורכבים בכתיבות קוד. כמו כן, ניתן לשנות את הפורמט של הפלט כדי להתאים לצורך המסוים שלנו.

### דוגמאות נוספות:
- ניתן להשתמש בפרמטר `label` כדי לתייג את הפלט בצורה מובנת יותר.
- ניתן להוסיף נתונים נוספים ל `IO.inspect` כמו `opts` ו `detailed: true` כדי לקבל מידע נוסף על האובייקט.
- ניתן להשתמש ב `IO.inspect` עם בלוקי קוד במקום עם משתנים בודדים.

# ראה גם
- [קוד אליקסר מדומיין](https://elixir-lang.org/getting-started/debugging.html)
- [מדריך לבכנות אליקסר של JetBrains](https://www.youtube.com/watch?v=uyi2UYnM_m8)
- [מדריך הדגמה של IO.inspect באליקסר](https://hexdocs.pm/elixir/IO.html#inspect/2)