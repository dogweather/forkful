---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:36.495210-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1Lua, \u05D9\
  \u05E6\u05D9\u05E8\u05EA \u05DE\u05E2\u05E8\u05DA \u05D0\u05E1\u05D5\u05E6\u05D9\
  \u05D0\u05D8\u05D9\u05D1\u05D9 (\u05D0\u05D5 \u05D8\u05D1\u05DC\u05D4, \u05D1\u05E9\
  \u05E4\u05EA Lua) \u05D4\u05D5\u05D0 \u05E4\u05E9\u05D5\u05D8. \u05D0\u05EA\u05D4\
  \ \u05DE\u05D5\u05D5\u05EA\u05E8 \u05E2\u05DC \u05D4\u05D0\u05D9\u05E0\u05D3\u05E7\
  \u05E1\u05D9\u05DD \u05D4\u05DE\u05E1\u05E4\u05E8\u05D9\u05D9\u05DD \u05D4\u05E8\
  \u05D2\u05D9\u05DC\u05D9\u05DD \u05DC\u05D8\u05D5\u05D1\u05EA \u05DE\u05E4\u05EA\
  \u05D7\u05D5\u05EA \u05DE\u05D1\u05D7\u05D9\u05E8\u05EA\u05DA. \u05EA\u05E8\u05D0\
  \u05D5 \u05D0\u05EA \u05D6\u05D4."
lastmod: '2024-03-13T22:44:39.539018-06:00'
model: gpt-4-0125-preview
summary: "\u05D1Lua, \u05D9\u05E6\u05D9\u05E8\u05EA \u05DE\u05E2\u05E8\u05DA \u05D0\
  \u05E1\u05D5\u05E6\u05D9\u05D0\u05D8\u05D9\u05D1\u05D9 (\u05D0\u05D5 \u05D8\u05D1\
  \u05DC\u05D4, \u05D1\u05E9\u05E4\u05EA Lua) \u05D4\u05D5\u05D0 \u05E4\u05E9\u05D5\
  \u05D8."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E2\u05E8\u05DB\u05D9\u05DD\
  \ \u05D0\u05E1\u05D5\u05E6\u05D9\u05D0\u05D8\u05D9\u05D1\u05D9\u05D9\u05DD"
weight: 15
---

## איך לעשות:
בLua, יצירת מערך אסוציאטיבי (או טבלה, בשפת Lua) הוא פשוט. אתה מוותר על האינדקסים המספריים הרגילים לטובת מפתחות מבחירתך. תראו את זה:

```Lua
-- יצירת מערך אסוציאטיבי
userInfo = {
  name = "Jamie",
  occupation = "Adventurer",
  level = 42
}

-- גישה לאלמנטים
print(userInfo["name"]) -- מדפיס Jamie
print(userInfo.occupation) -- מדפיס Adventurer

-- הוספת זוגות מפתח-ערך חדשים
userInfo["hobby"] = "Coding"
userInfo.favLang = "Lua"

-- עיבור על המערך האסוציאטיבי
for key, value in pairs(userInfo) do
  print(key .. ": " .. value)
end
```

פלט:
```
Jamie
Adventurer
name: Jamie
occupation: Adventurer
level: 42
hobby: Coding
favLang: Lua
```

החלק המגניב? אתה מתקשר עם הנתונים באמצעות מפתחות בעלי משמעות עבורך, הופך את הקוד לקריא ולתחזק יותר.

## צלילה עמוקה
כשLua נכנס לזירה, הוא הציג טבלאות כמבנה נתונים המשמש לכל דבר, מהפכני את האופן שבו מפתחים מנהלים נתונים. בניגוד לחלק מהשפות שבהן מערכים אסוציאטיביים ומערכים הם ישויות נפרדות, טבלאות Lua משמשות גם כך וגם כך, מפשטות את נוף מבנה הנתונים.

מה שהופך את טבלאות Lua לכל כך עוצמתיות הוא הגמישות שלהן. עם זאת, הגמישות הזו באה במחיר של השפעות אפשריות על הביצועים, במיוחד עם סטים גדולים של נתונים שבהם מבנה נתונים מיוחד יותר עשוי להיות מועדף מבחינת יעילות.

למרות שLua אינו תומך באופן טבעי במבני נתונים יותר קונבנציונליים, כמו רשימות מקושרות או מפות האש, גמישות מבנה הטבלאות אומרת שאתה יכול לממש אותם באמצעות טבלאות אם יש צורך. רק זכור: עם כוח גדול באה אחריות גדולה. השתמש בגמישות בחוכמה כדי לשמור על ביצועים וקריאות של הקוד שלך.
