---
title:                "גילוי מספרים אקראיים"
date:                  2024-01-20T17:49:53.131640-07:00
model:                 gpt-4-1106-preview
simple_title:         "גילוי מספרים אקראיים"
programming_language: "Lua"
category:             "Lua"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?
יצירת מספרים אקראיים בתכנות היא תהליך שבו נוצרים מספרים שאינם צפויים. תוכניתנים משתמשים בכך למגוון מטרות, כולל משחקים, בדיקות, וסימולציות.

## איך לעשות:
ב-Lua, הנה איך תוצר מספר אקראי:

```lua
math.randomseed(os.time()) -- זרע להתחלה חד פעמית
print(math.random())       -- מספר אקראי בין 0 ל-1
print(math.random(10))     -- מספר אקראי שלם בין 1 ל-10
print(math.random(5, 20))  -- מספר אקראי שלם בין 5 ל-20
```

דוגמא לפלט:
```
0.0012512588885153
7
17
```

## טבילה עמוקה:
הפונקציה `math.randomseed` מאתחלת את גנרטור המספרים האקראיים של Lua עם זרע. אם לא תאתחל, תקבל יחסיות את אותו סדר של מספרים בכל פעם. השימוש ב-`os.time()` מבטיח שהזרע יהיה שונה בכל פעם.

בעבר, שיטות שונות נעשו בתכנות ליצירת מספרים אקראיים, כולל טכניקות כמו ריבועים אמצעיים ושימוש בתרמילוגיה קריפטוגרפית להבטחת אקראיות אמיתית.

למעשה, מספרים אקראיים במחשבים לא באמת "אקראיים"; הם "פסבדו-אקראיים" מאחר שהם נוצרים על ידי אלגוריתם דטרמיניסטי. אך לרוב השימושים השגרתיים, הם "אקראיים" מספיק.

## גם ראה:
- [Lua 5.4 Reference Manual - math library](https://www.lua.org/manual/5.4/manual.html#6.7)
- [Wikipedia article on Pseudorandom number generators](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
