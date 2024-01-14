---
title:                "Kotlin: עובדים עם json"
simple_title:         "עובדים עם json"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## למה

JSON הוא אחד מהדרכים הפופולריות ביותר לשמירת והחלפת מידע בין אפליקציות שונות. זה מתאים בעיקר לתקשורת בין רשתות או אפליקציות עם שרתים מרוחקים. בהינתן כמות גדולה של נתונים ופעולות מולהם, JSON יכול להיות כלי חזק עבור סידור והעברת המידע.

## איך לעבוד עם JSON בקוד הכזה לשמור את הנתונים כך שיהיו נגישים בצורה חזותית וקריאה. באמצעות הפונקציה `JSONObject()` ניתן ליצור אובייקט מתאים לנתונים שנתקבלים. ניתן להגדיר את הנתונים באמצעות פעולת השמה ולתת למשתמש להכניס את הנתונים הרצויים באופן ידני על ידי הזנת פרמטרים שונים. כל הנתונים ייכנסו לתוך מערך אחד גדול וניתן יהיה לגשת אליהם בצורה פשוטה.

```
Kotlin
val obj = JSONObject()
obj.put("name", "John")
obj.put("age", 30)
obj.put("country", "Israel")
```

המחרוזת מחזירה את התוצאות הבאות:

```
{"name":"John", "age": 30, "country": "Israel"}
```

## ירידה לעומק

בנוסף ליצירת נתונים, ישנם כמה פונקציות נוספות בקוד כמו `putIfAbsent()` ו`remove()` שמאפשרות עריכה ומחיקת פריטים במערך. כמו כן, ניתן להשתמש בפעולת `toMap()` כדי להמיר את הנתונים למבנה מפת המפתחות של התוכן. פונקציות אלו מאפשרות נגישות נוחה לנתונים למטרות עריכה ושליפה.

## ראו גם

- [התיעוד המלא של Kotlin עבור JSON](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.collections/to-map.html)
- [מדריך מלא לעבודה עם JSON בקוד Kotlin](https://www.tutorialspoint.com/kotlin/kotlin_json_processing.htm)
- [מדריך על