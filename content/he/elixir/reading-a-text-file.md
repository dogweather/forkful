---
title:                "Elixir: קריאת קובץ טקסט"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

##למה
פתיחת קובץ טקסט היא חלק חשוב בתוכנות פיתוח וחידוש. קריאת קבצים מאפשרת לנו לקרוא ולעבוד עם מידע ממקור מסוים וליצור מילות קוד דינמיות לפי הצורך. אליקסיר היא שפת תכנות נפוצה היום, וחיבור של זה עם קריאת טקסט מאפשר לנו ליצור תוכניות מתקדמות ויעילות.

##כיצד לעשות זאת
האלמנט המרכזי בקריאת קבצי טקסט באליקסיר הוא הפונקציה "File.read/1". ניתן לקלוט את שם הקובץ כפרמטר והפונקציה תחזיר כתובת מקור המידע שנמצא בקובץ כסטרינג. ניתן להשתמש בפונקציות נוספות כמו "IO.write/2" ו"String.split/2" כדי לעבוד עם המידע וליצור תוצאות מתאימות.

```elixir
file_data = File.read("test.txt") #קריאת הקובץ ושמירת התוכן במשתנה file_data
IO.puts(file_data) #הדפסת התוכן שנמצא בקובץ
result = String.split(file_data, ",") #פיצול הקובץ לפי נקודות שבירה במשתנה result
IO.inspect(result) #הדפסת תוצאת הפיצול לצורך בדיקה
```

###פלט

קבצי טקסט יכולים להכיל מידע ממורכז או מסודר. ניתן להשתמש בפונקציות כמו "Enum.each/2" ו"IO.puts/2" כדי לעבוד ולהדפיס את המידע כפי שנדרש.

```elixir
file_data = File.read("test.txt")
result = String.split(file_data, ",")
Enum.each(result, fn line -> #עבור כל שורה בתוך המשתנה result
  IO.puts(line) #הדפס את השורה התואמת
end)
```

###מתעמקים עומק

קריאת קבצי טקסט באליקסיר יכולה להיות פעולה מאתגרת במקרים מסוימים. ניתן ל