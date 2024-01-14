---
title:                "Elixir: המרת מחרוזת לאותיות קטנות"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מדוע

המרת סטרינג לאותיות קטנות היא דבר רגיל ולא נדרש בכל פעם שאנו מתקדמים בתכנות. אך בעבור חוויית תכנות טובה יותר ולהימנעות מטעויות מיותרות, ידע על כך יכול להיות שימושי. במאמר זה נדבר על נושא זה בקצרה ונסביר כיצד לבצע את הפעולה באמצעות שפת אליקסיר.

## איך לעשות

תחילה נצטרך להגדיר את הסטרינג שנרצה להמיר לאותיות קטנות באמצעות הפונקציה `String.downcase/1`. לדוגמה: 

```Elixir
string = "ELIXIR IS AWESOME"
String.downcase(string)
```

פלט הפונקציה יחזיר סטרינג חדש שכולל את אותיות הסטרינג המקורי באותיות קטנות:

```Elixir
"elixir is awesome"
```

עוד דרך לעשות זאת היא על ידי שימוש בחיוב או בשלילה של האותיות בסטרינג על ידי הפונקציות `String.upcase/1` או `String.capitalize/1`. ניתן לראות את פלט הפונקציות באמצעות הקוד הבא:

```Elixir
String.upcase("elixir is awesome")
String.capitalize("elixir is awesome")
```
```Elixir
"ELIXIR IS AWESOME"
"Elixir is awesome"
```

כעת ננסה להמיר את הסטרינג לאותיות קטנות ולהשאר רק עם האותיות הראשונות בגודל גדול. נשתמש פה בשילוב של הפונקציות `String.downcase/1` ו-`String.capitalize/1` כך:

```Elixir
"elixir is awesome" |> String.downcase() |> String.capitalize()
```

פלט הפונקציה יחזיר סטרינג חדש שכולל את האות הראשונה באותיות גדולות, ואת כל האותיות האחרות באותיות קטנות:

```Elixir
"Elixir is awesome"
```

בכל מקרה, לאורך פיתוח תוכניות באליקסיר נבלט כמה פעמים הרשות לעביר מנהלה היא מנהל