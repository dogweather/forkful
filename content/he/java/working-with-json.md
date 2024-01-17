---
title:                "עובדים עם json"
html_title:           "Java: עובדים עם json"
simple_title:         "עובדים עם json"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/working-with-json.md"
---

{{< edit_this_page >}}

## מה זה ולמה?
עבודה עם JSON היא תהליך תכנותי שמאפשר למפתחים לקרוא, להעביר ולהצטרף לנתונים בפורמט מסודר ויעיל. זה נמצא בשימוש נרחב על ידי מפתחי תוכנה, במיוחד כאשר מדובר בפיתוח אפליקציות ואתרים מרובי-קטעים.

## איך לעשות זאת:
תחת הקוד של ```Java ... ```, תוכלו למצוא דוגמאות איך לעבוד עם JSON כדי לקרוא, לכתוב ולהוסיף נתונים. להמשיך קריאת הדוגמאות כדי לראות את הפלט המתאים.

```Java
// קוראים מידע מקובץ JSON
JSONObject json = new JSONObject(readFile("user.json"));
// מדפיסים את המאפיינים של המשתמש
System.out.println("שם: " + json.get("שם"));
System.out.println("מין: " + json.get("מין"));
System.out.println("גיל: " + json.get("גיל"));
```

```Java
// כותבים נתונים לקובץ JSON חדש
JSONObject json = new JSONObject();
json.put("כינוי", "משתמש1");
json.put("גיל", 30);
json.put("מין", "זכר");

try (FileWriter file = new FileWriter("user.json")) {
    file.write(json.toJSONString());
}
```

## טפסים נעוצים:
JSON נוצר בשנת 2001 כפתרון לבעיות קריאה וכתיבה של נתונים מורכבים בפורמט פשוט ובמהירות. פופולריות שלו עלתה עם התפתחות רחבה של טכנולוגיות האינטרנט ושימוש בו כפורמט אחד תקני עבור REST API. כיום, ישנם יישויות רבות אחרות המתכתבות עם JSON כמו XML ו-YAML.

## ראו גם:
- [JSON official website](https://www.json.org/json-en.html)
- [Baeldung article about JSON in Java](https://www.baeldung.com/java-org-json)