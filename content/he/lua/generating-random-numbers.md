---
title:                "גירוד מספרים אקראיים"
html_title:           "Haskell: גירוד מספרים אקראיים"
simple_title:         "גירוד מספרים אקראיים"
programming_language: "Lua"
category:             "Lua"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?
התוכניתים מייצרים מספרים אקראיים  כייף למשללת שימושים, בהם משחקים, ניסויים, אבטחת מידע ועוד. זה מספק חשיפה חפה מזמן ותנאים תחביביים למוצא של התוכנה. 

## איך לעשות:
כדי ליצור מספרים אקראיים בשפת Lua, משתמשים בפונקציה math.random(). ‏‏נראה את זה בדרך של דוגמה של קוד:

```Lua
math.randomseed(os.time())

local random_number = math.random(1, 10)

print(random_number)
```

בריצה של הקוד הזה יודפיס מספר אקראי בין 1 ל-10.

## השקפה עמוקה
ב- Lua, מנגנון המספרים האקראיים מבוסס על מימוש של אלגוריתם 'Mersenne Twister', שהוא גרעין מספר אקראי שמשמש ברוב המערכות המודרניות. שימוש ב `math.randomseed(os.time())` מבטיח שלא תקבל את אותם המספרים בכל פעם שהתוכנית נדלקת. החלק `os.time()` מגריל מספר זרע מבוסס זמן.
אם אתה צריך מספרים אקראיים שמתפקדים בצורה דיטרמיניסטית לבדיקות, אתה יכול לשקול להשתמש ב `math.randomseed()` עם זרע קבוע.

## קישורים משולבים
1. התיעוד המקורי של Lua 'math.random ()': http://www.lua.org/manual/5.3/manual.html#6.7
2. ויקיפדיה על 'Mersenne Twister': https://en.wikipedia.org/wiki/Mersenne_Twister
3. הספר 'Programming in Lua': https://www.lua.org/pil/19.2.html