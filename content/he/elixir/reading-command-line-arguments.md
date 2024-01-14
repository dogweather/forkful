---
title:                "Elixir: קריאת מתוני שורת פקודה"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## למה

ברגע שאנו מתחילים לפתח תוכניות ב־Elixir, אנו מוצאים את עצמנו משתמשים בפעולת הקומנד ליין המוכרת ומוערכת על פני כל מערכת ההפעלה. כיצד פועלת פעולה זו ולמה היא כל כך חשובה? בכתבה הבאה, נכיר את קריאת הפרמטרים מקומנד ליין באמצעות השפה המוכרת לכלל הקודנים שלנו - Elixir. 

## כיצד לעשות זאת

בשפת Elixir, ישנם שני דרכים עיקריות לקרוא פרמטרים מהקומנד ליין. האופציה הראשונה היא להשתמש בספריית המובנית `OptionParser`, שתאפשר לנו לקרוא פרמטרים שונים בקלות ובפשטות. הנה דוגמא פשוטה:

```Elixir
# התחלת קוד דוגמא
defmodule CommandLine do
   # צור כתובת IP דיפולטיבית
   # address = "0.0.0.0"
   address = OptionParser.parse!(OptionParser.new, "-a", "--address")
   IO.puts "כתובת ה־IP שלי היא: #{address}"
end

# סוף קוד דוגמא 

#קודם, נקטיל את הפרמטרים באמצעות קומנד ליין
elixir ./command_line.exs --address="127.0.0.1"

#פלט:
כתובת ה־IP שלי היא: 127.0.0.1 
```

באופן דומה, ישנה גם דרך לקרוא את הפרמטרים ישירות בכתובת הסקריפט, באמצעות תיבת המבוא ופונקציות המובנות. לדוגמא:

```Elixir
# התחלת קוד דוגמא
defmodule CommandLine do
   # קרא את הפרמטר הראשון מתוך המערך 
   # args = ["elixir", "./script.exs", "--name", "John"]
   args = System.argv
   
   # הדפס את השם 
   IO.puts "שלום #{args[3]}"
end

# קוד דוגמא

# פלט:
שלום ג'ון 
```

במידה ובמקרה שיש לנו יכולות קוד מורחבות יותר, נוכל להשתמש בעשרות פונקציות נוספות ה