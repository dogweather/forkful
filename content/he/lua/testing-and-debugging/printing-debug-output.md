---
aliases:
- /he/lua/printing-debug-output/
date: 2024-01-20 17:53:19.889459-07:00
description: "\u05D0\u05B8\u05D5\u05D8\u05E4\u05BC\u05D5\u05D8 \u05D3\u05D9\u05D1\u05D0\
  \u05D2 \u05D6\u05D4 \u05DB\u05DC\u05D9 \u05DC\u05D4\u05D3\u05E4\u05E1\u05EA \u05D4\
  \u05D5\u05D3\u05E2\u05D5\u05EA \u05E9\u05D2\u05D9\u05D0\u05D4 \u05D5\u05E4\u05E8\
  \u05D8\u05D9 \u05DE\u05E2\u05E7\u05D1 \u05D1\u05D6\u05DE\u05DF \u05E8\u05D9\u05E6\
  \u05EA \u05D4\u05EA\u05D5\u05DB\u05E0\u05D4. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\
  \u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D6\u05D4 \u05DB\
  \u05D3\u05D9 \u05DC\u05D1\u05D3\u05D5\u05E7 \u05DE\u05D4 \u05E7\u05D5\u05E8\u05D4\
  \ \"\u05DE\u05EA\u05D7\u05EA \u05DC\u05DE\u05DB\u05E1\u05D4\", \u05D5\u05DC\u05D0\
  \u05EA\u05E8 \u05D1\u05D0\u05D2\u05D9\u05DD."
lastmod: 2024-02-18 23:08:52.979264
model: gpt-4-1106-preview
summary: "\u05D0\u05B8\u05D5\u05D8\u05E4\u05BC\u05D5\u05D8 \u05D3\u05D9\u05D1\u05D0\
  \u05D2 \u05D6\u05D4 \u05DB\u05DC\u05D9 \u05DC\u05D4\u05D3\u05E4\u05E1\u05EA \u05D4\
  \u05D5\u05D3\u05E2\u05D5\u05EA \u05E9\u05D2\u05D9\u05D0\u05D4 \u05D5\u05E4\u05E8\
  \u05D8\u05D9 \u05DE\u05E2\u05E7\u05D1 \u05D1\u05D6\u05DE\u05DF \u05E8\u05D9\u05E6\
  \u05EA \u05D4\u05EA\u05D5\u05DB\u05E0\u05D4. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\
  \u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D6\u05D4 \u05DB\
  \u05D3\u05D9 \u05DC\u05D1\u05D3\u05D5\u05E7 \u05DE\u05D4 \u05E7\u05D5\u05E8\u05D4\
  \ \"\u05DE\u05EA\u05D7\u05EA \u05DC\u05DE\u05DB\u05E1\u05D4\", \u05D5\u05DC\u05D0\
  \u05EA\u05E8 \u05D1\u05D0\u05D2\u05D9\u05DD."
title: "\u05D4\u05D3\u05E4\u05E1\u05EA \u05E4\u05DC\u05D8 \u05DC\u05E0\u05D9\u05E4\
  \u05D5\u05D9 \u05D1\u05D0\u05D2\u05D9\u05DD"
---

{{< edit_this_page >}}

## מה ולמה?
אָוטפּוט דיבאג זה כלי להדפסת הודעות שגיאה ופרטי מעקב בזמן ריצת התוכנה. תכניתנים משתמשים בזה כדי לבדוק מה קורה "מתחת למכסה", ולאתר באגים.

## איך לעשות:
```Lua
-- פשוט תדפיסו עם print:
print("Debug: הערך של x הוא", x)

-- אם רוצים להדפיס משהו במהלך ריצת פונקציה:
function doSomething(a, b)
  print("Debug: a + b =", a + b)
  -- עוד קוד כאן
end

-- דוגמא לפלט
Debug: הערך של x הוא    10
Debug: a + b =            25
```

## צלילה עמוקה
הדפסת דיבאג היא שיטה ותיקה ונפוצה לאיתור תקלות. בימים שקדמו לעידן הממשק הגרפי, ההדפסה לקונסול הייתה הדרך היחידה להבין מה התוכנית עושה. כיום ישנם כלים חזקים יותר כמו מנגנוני לוגינג מתקדמים ומנגנוני דיבאגינג, אבל print נשאר פשוט וישיר לשימוש לצרכים מיידיים. על אף זאת, שימוש יתר בהדפסת דיבאג עלול להצפות את הקונסול ולעשות קשה יותר למצוא את הנתונים הרלוונטיים.

## גם ראו
- [Lua 5.4 Reference Manual](https://www.lua.org/manual/5.4/)
- [Programming in Lua (first edition)](https://www.lua.org/pil/contents.html)
- [Debugging in Lua with ZeroBrane Studio](https://studio.zerobrane.com/doc-lua-debugging)
