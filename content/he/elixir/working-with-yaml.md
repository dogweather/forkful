---
title:                "עבודה עם YAML."
html_title:           "Elixir: עבודה עם YAML."
simple_title:         "עבודה עם YAML."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## למה

למה מישהו יתעקש לעבוד עם YAML? תפקידו העיקרי של YAML הוא לטפל בנתונים מבני הידע, וכך מאפשרת הוא למשתמשים לקרוא ולנתח מידע למטרות שונות, כולל פתרון בעיות ופיתוח יישומים מתקדמים.

## איך לעשות 

תחילה, אנחנו צריכים להתקין את חבילת ה-YAML שלא שלנו במכשיר שבו אנחנו רוצים לעבוד. יש לנו כמה אפשרויות שיכולות לעזור לנו בכך. למשל:

```Elixir
defp deps do
  [
    {:yaml_elixir, "~> 0.4"}
  ]
end
```

לאחר מכן, אנחנו נוכל להשתמש בפונקציות של YAML על מנת לקרוא ולכתוב נתונים לקובץ YAML. ניתן לראות כמה דוגמאות לפעולות אלו בקוד הבא:

```Elixir
# קריאת נתונים מקובץ YAML
YamlElixir.load_file("data.yml")

# כתיבת נתונים לקובץ YAML
data = %{name: "John", age: 30}
YamlElixir.dump(data, "output.yml")
```

בנוסף, YAML מספק מתשמשים עם חלק מהאפשרויות המתקדמות כמו שימוש בדירוגים, צבירת תווים והגדרת מפתחות ריקות.

## העומק

בנוסף לפעולות הבסיסיות של YAML, ניתן לעשות לבדוק ולהמיר את הנתונים בעזרת מספר ספריות נוספות. למשל, ספריות כגון YAMLixir ו-YamlEcto מאפשרות לנו לבצע פעולות מתקדמות יותר עם נתוני YAML, כולל יצירת מודלים ועדכון למסדי נתונים.

בנוסף, כדאי לבחון את תכנון פורמט הנתונים שלנו בקובץ YAML כדי לקבל את התוצאות המבוקשות בתוך היישומים שלנו.

## ראה גם

- [מסמך ה-YAML הרשמי](https://yaml.org/)
- [ספריית YAMLixir](https://hexdocs.pm/yaml_elixir/api