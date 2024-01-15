---
title:                "עובדים עם json"
html_title:           "Elixir: עובדים עם json"
simple_title:         "עובדים עם json"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## למה

JSON הוא פורמט נתונים פופולרי ופשוט לשימוש, המאפשר תקשורת בין שפות תכנות שונות. אליקסיר מציע כלים יעילים לעבודה עם JSON ויכולת לטפל בו בקלות וביעילות.

## איך לעבוד עם JSON באליקסיר?

כדי להתחיל לעבוד עם JSON באליקסיר, ניתן להשתמש בפונקציות `import` ו `poison.decode` כדי להמיר את נתוני JSON למבנה מבושם באליקסיר. כאן מספר דוגמאות לכיצד לעבוד עם מבנה נתונים JSON באליקסיר:

```
# ניתוח נתוני JSON מורכבים:

json = %{"name": "John", "age": 30, "pets": [%{"name": "Spot", "age": 5}, %{"name": "Spike", "age": 3}]}

# פקודת ניקוי:

clean_json = Poison.decode!(json)

# מבנה החזרה יחזיר שם וגיל של כל משתמש, תוך התעלמות מהחיות המחמד שלהם:

%{name: name, age: age} = clean_json
IO.puts "#{name} is #{age} years old."
# פלט: John is 30 years old.

# בנייה של יוחסנים בתוך התוכנית:

pets = Enum.map(clean_json.pets, fn(pet) -> pet.name end)
# בחזרה לתוכנית עיקרית כאשר נמצאים מספר חיות מחמד:
IO.puts "John has #{pets} as pets."
# פלט: John has ['Spot', 'Spike'] as pets.
```

## עימוד עמוק

כאשר משתמשים באליקסיר לעיבוד נתוני JSON, יש לשים לב לכמה נקודות חשובות. למעשה, כדי לעבוד עם JSON באליקסיר, נתונים אלו חייבים להיות מבנה נתונים לא מסורתי. נתוני JSON חייבים להיות בתבנית תקינה ונמצאים בתוך תוכן Poison ছולו. ניתן למצוא מידע נוסף על איך לעבוד עם JSON באליקסיר ב[תיעוד הרשמי של Poison](https://hexdocs.pm/poison/).

## ראו גם

למידע נוסף על עבודה עם אליקסיר, ניתן לבחון את [