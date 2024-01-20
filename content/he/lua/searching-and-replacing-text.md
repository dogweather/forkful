---
title:                "חיפוש והחלפת טקסט"
html_title:           "Elm: חיפוש והחלפת טקסט"
simple_title:         "חיפוש והחלפת טקסט"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## מה זה ולמה? 

חיפוש והחלפה של טקסט הם פעולות שבהן מחרוזת מסוימת בטקסט מוחלפת במחרוזת אחרת. תכנתים מבצעים את הפעולות האלה כדי לשנות את התוכן של טקסט, להתאים אותו לצרכים מסוימים, או לשנות מחרוזות של קוד.

## איך לעשות: 

בלוע, ניתן לחפש ולהחליף טקסט באמצעות שימוש בפונקציה `string.gsub`. נסו את הדוגמה הבאה:
```Lua
s = "ברוך הבא לעולם של Lua"
new_s, n = string.gsub(s, "Lua", "Lua programming")
print(new_s)  -- prints "ברוך הבא לעולם של Lua programming"
print(n)  -- prints 1
```
הפונקציה מחזירה שני ערכים: המחרוזת החדשה ומספר ההחלפות שנערכו.

## צלילה עמוקה: 

על פי ההיסטוריה, צורת החיפוש וההחלפה של Lua מבוססת על המערכת המוכרת regex שנמצאת בשפות רבות נוספות. במקרה שאתה מחפש אלטרנטיבות, אתה יכול להתאמת את הפונקציות `string.match` או `string.find` ואז להחליף מדובר במנגנון בלעדי ל-Lua. 

מההיבט המימוש, החיפוש הראשוני הוא לפי המופע הראשון של הטקסט לחיפוש ואז מתבצעת ההחלפה. הפעולה מתרחשת לכל המופעים בטקסט. זה מעשה הסיבה שאנחנו מקבלים מספר של ההחלפות שנערכו כשהוא מחזיר את הערך.

## ראו גם:

1. [Lua: string.gsub](https://www.lua.org/manual/5.3/manual.html#pdf-string.gsub) - הסבר מלא של הפונקציה `string.gsub`.
2. [Lua: string.find](https://www.lua.org/manual/5.3/manual.html#pdf-string.find) - למידה נוספת על השילוב של `string.find` והחלפה.
3. [Lua: string.match](https://www.lua.org/manual/5.3/manual.html#pdf-string.match) - השימוש ב`string.match` למציאת מחרוזות במחרוזת אחרת.