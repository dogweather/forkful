---
title:                "כתיבת בדיקות"
html_title:           "Lua: כתיבת בדיקות"
simple_title:         "כתיבת בדיקות"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/lua/writing-tests.md"
---

{{< edit_this_page >}}

# מה ולמה? 
לכתוב בדיקות זה פשוט לכתוב קטעי קוד שמוודאים שהתוכנה שכתבנו עובדת כפי שצריך. פרוגרמים עושים את זה כדי לוודא שהתוכנה שכתבנו תעבוד כמו שציפינו שתעבוד, תוך כדי איתור ותיקון כל בעיות שיכולות להיות קיימות בקוד שלנו. 

# איך לעשות את זה:

```lua
-- הגדרת פונקציה לבדיקה של טבלאות נתונים
function table_test()
  -- יצירת טבלה עם ערכים לבדיקה
  local test_table = {"apple", "banana", "orange"} 
  
  -- הדפסת כמות הערכים בטבלה
  print(#test_table) -- ציפוי לקבלת תוצאה - 3
  
  -- הפיכת אחד הערכים לאות גדולה
  test_table[2] = string.upper(test_table[2])
  
  -- הדפסת טבלה מעודכנת
  print(table.concat(test_table, ", ")) -- ציפוי לקבלת תוצאה - apple, BANANA, orange
  
  -- בדיקה אם הטבלה מכילה את הערך "kiwi"
  if table.contains(test_table, "kiwi") then
    print("Kiwi is in the table!")
  else
    print("Kiwi is not in the table")
  end
end

-- הגדרת פונקציה כדי להריץ את הבדיקה שלנו
function run_test()
  -- קריאה לפונקציה המבצעת את הבדיקות
  table_test()
  -- הדפסת הודעת הצלחה כאשר הבדיקות הושלמו בהצלחה
  print("All tests passed!")
end

-- הרצת הבדיקות
run_test()
```

Output:
```
3
apple, BANANA, orange
Kiwi is not in the table
All tests passed!
```

# חקירה מעמיקה:
- תוכניות בדיקות נמצאות בשימוש כבר מזמן רב, אף לפני פיתוח דרכי אגודל של תוכנה. הן נכתונות כדי לוודא שהתוכנה עובדת כפי שצריך, כך שאפשר למנוע בעיות ותקלות בפיתוח התוכנה.

- ישנן כמה דרכים שונות לכתוב בדיקות, כגון בדיקות ידניות ובדיקות אוטומטיות. בדיקות ידניות נעשות על ידי הפרוגרמר עצמו, במידה והוא עשה שינויים בקוד. בדיקות אוטומטיות מתבצעות באמצעות כלי תוכנה שאוטומטית מבצעים בדיקות על הקוד.

- בכדי לכתוב בדיקות אוטומטיות בלואה, ניתן להשתמש בכלי תוכנה כמו LuaUnit או busted. חשוב להיות יציבים בכתיבת הקוד ולכתוב בדיקות הרמוניות כמו שאפשר, כדי לעזור לכם לזהות ולתקן בעיות במהלך הפיתוח של התוכנה שלכם.

# ראו גם:


- [LuaUnit - כלי תוכנה לבדיקות אוטומטיות בלואה](https://github.com/bluebird75/luaunit)