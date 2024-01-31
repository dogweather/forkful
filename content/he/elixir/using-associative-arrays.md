---
title:                "שימוש במערכים אסוציאטיביים"
date:                  2024-01-30T19:11:20.499729-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש במערכים אסוציאטיביים"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

ב-Elixir, מערכים אסוציאטיביים, המכונים Maps, הם אוספים של זוגות מפתח-ערך בהם מפתח ייחודי מצביע על ערך. הם מאוד שימושיים לאחסון ואיתור נתונים במהירות, מה שהופך את הקוד שלך לנקי יותר ואת חייך לקלים יותר.

## איך ל:

יצירת Map היא פשוטה. משתמשים בתחביר `%{}`, כמו כן:

```elixir
my_map = %{"name" => "Alex", "age" => 32}
IO.inspect(my_map)
```

גישה לערכים מתבצעת על ידי שימוש במפתחות:

```elixir
IO.puts my_map["name"]
```
פלט: `Alex`

להוספה או עדכון ערכים, ניתן להשתמש בפונקציה `Map.put/3`:

```elixir
updated_map = Map.put(my_map, "location", "NY")
IO.inspect(updated_map)
```
פלט: `%{"age" => 32, "location" => "NY", "name" => "Alex"}`

הסרת מפתחות פשוטה בדיוק עם `Map.delete/2`:

```elixir
trimmed_map = Map.delete(updated_map, "age")
IO.inspect(trimmed_map)
```
פלט: `%{"location" => "NY", "name" => "Alex"}`

## צלילה עמוקה

Maps ב-Elixir הם התפתחות של סוגים ישנים יותר של אחסון מפתח-ערך, כמו Hashes ב-Ruby או Dictionaries ב-Python. הם מאפשרים חיפושים והכנסות יעילים יותר, מה שהופך אותם לבחירה המועדפת לתכנות Elixir מודרני. כדאי לציין שלפני Maps, Elixir השתמשה במודולים HashDict ו-Dict, שכבר אינם בשימוש כיום.

עם זאת, לסינריואים שדורשים נתונים מסודרים, ייתכן שתחפשו אחר רשימות מילות מפתח ב-Elixir. אלו הן רשימות של טאפלים, יעילות לאוספים קטנים אך לא כל כך ידידותיות לביצועים עבור סטים גדולים של נתונים כמו Maps.

חשוב לזכור ש-Maps מאחסנים את מפתחותיהם במבנה "שטוח", מה שהופך את הגישה ישירות לערכים מקוננים למעט מסובכת. עבור קינון עמוק, ייתכן שתשקלו גישה מובנית דרך הפונקציות `get_in`, `put_in`, `update_in`, ו-`get_and_update_in`, המאפשרות גישה יותר דינמית להתמודדות עם נתונים מקוננים.

לסיכום, בעוד ש-Maps הם הבחירה שלך לצרכי מערכים אסוציאטיביים ב-Elixir, השפה מציעה מגוון עשיר של מבני נתונים לכל סצנה, מה שמעודד אותך לבחור את הכלי הנכון למשימה.
