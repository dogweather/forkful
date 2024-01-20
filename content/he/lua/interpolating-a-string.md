---
title:                "אינטרפולציה של מחרוזת"
html_title:           "Arduino: אינטרפולציה של מחרוזת"
simple_title:         "אינטרפולציה של מחרוזת"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/interpolating-a-string.md"
---

{{< edit_this_page >}}

## מה זה ולמה?

אינטרפולציה של מחרוזת היא תהליך שבו משלבים ערכים ממשתנים תוך כדי בניית מחרוזת. לחלופין, מתכנתים מבצעים את זה כדי ליצור מחרוזת דינמית מתוך ערכים משתנים.

## איך עושים את זה:

ב-Lua, אנחנו יכולים לבצע את זה באמצעות הפונקציה `string.format`. נשים לב לדוגמה:

```Lua
local name = "David"
local age = 35
local sentence = string.format("שלום, שמי %s ואני בן %d", name, age)

print(sentence) -- שלום, שמי David ואני בן 35
```

## צלילה עמוקה:

אינטרפולציה של מחרוזת היא מונח שהומצע בשפת Perl בשנות ה-80. החלופה לתהליך זה ב-Lua היא שימוש בחיבור מחרוזת שמוביל למחרוזת מרובה:

```Lua
local sentence = "שלום, שמי " .. name .. " ואני בן " .. age
```

אומנם `string.format` מצדיק את עצמו ביותר כאשר נדרש יצירת מחרוזת מרובה של משתנים מתחלפים.

## ראה גם:

* התיעוד של Lua בנושא `string.format`: https://www.lua.org/manual/5.4/manual.html#6.4.2
* ספר הלימוד Lua - "Programming in Lua": http://www.lua.org/pil/20.html