---
title:                "Java: עובדים עם json"
simple_title:         "עובדים עם json"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/working-with-json.md"
---

{{< edit_this_page >}}

## כולם יודעים שפייתון היא אחת השפות המועדפות בעולם התכנות. למה אז כדאי ללמוד על JSON בכלל?

JSON היא תקן פופולרי לפריסת מידע מבוסס כתב למכונה (machine-readable). בעזרת JSON ניתן לארגז מידע בצורה מדויקת ונוחה לשימוש. היא נמצאת בשימוש במגוון מקרים, כגון יצירת אפליקציות מובייל או אתרים תומכי API.

## איך לעבוד עם JSON ב-Java?

עבודה עם JSON ב-Java פשוטה ונוחה. התחלה טובה היא להתקין ספריית JSON המתאימה להיישום שלך. לדוגמה, אם אתה משתמש במערכת הניהול MySQL, תוכל להתשמש בספריית JSON-Simple המתאימה לתכנית היישום שלך.

לחילופין, ניתן להשתמש בספרייה של Google, Gson. לאחר התקנה, ניתן לייצא את המידע לקובץ JSON עם פונקציות כמו "toJson" ולייבא אותו עם פונקציות כמו "fromJson". להלן דוגמה ליצירת מילון בעזרת Gson:

```Java
Gson gson = new Gson();
String json = "{ 'name' : 'John', 'age' : 30" }";
Map<String, Object> dictionary = gson.fromJson(json, HashMap.class);
System.out.println(dictionary);
```

פלט:
```
{name=John, age=30}
```
לדוגמה, ניתן לשנות את תוכן ה-JSON עד שסנכרון תעבור. כך, נוכל לעבוד עם מילונים בקלות, לשנות את הערכים ולייצא אותם חזרה לקובץ JSON.

## חקירה עמוקה: פורמט JSON והרצאה ל-String

JSON מרכיב לעומת התמונה בצורה דומה ל-Markdown. סוגי יסוד משתלבים בתקת הפורמט וניתן לעבור עליהם מתוך פונקציות כמו "getAsString" ו-"getAsInt". נמנע את היצירה של SQL למשתמשי C או התאמה על פי מקור המידע.

ניתן לאתר למקלדת שבידינו