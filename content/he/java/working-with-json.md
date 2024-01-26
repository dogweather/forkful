---
title:                "עבודה עם JSON"
html_title:           "Arduino: עבודה עם JSON"
simple_title:         "עבודה עם JSON"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/working-with-json.md"
---

{{< edit_this_page >}}

## מה ולמה?
JSON זה תסדיר להתכתבויות מידע, קל לקריאה וכתיבה גם על ידי מחשבים וגם על ידי בני אדם. מתכנתים משתמשים ב-Java כדי לנהל נתונים בפורמט JSON כי זה נפוץ, גמיש, וקל לשילוב ברשת.

## איך לעשות:
```java
import org.json.JSONObject;

public class JsonExample {
    public static void main(String[] args) {
        // יצירת JSON אובייקט
        JSONObject obj = new JSONObject();
        obj.put("name", "Yossi");
        obj.put("age", 30);
        obj.put("isProgrammer", true);

        // הדפסת ה-JSON לקונסול
        System.out.println(obj.toString());

        // קריאה מ-JSON אובייקט
        String name = obj.getString("name");
        int age = obj.getInt("age");
        boolean isProgrammer = obj.getBoolean("isProgrammer");

        System.out.println("Name: " + name);
        System.out.println("Age: " + age);
        System.out.println("Is Programmer: " + isProgrammer);
    }
}
```
פלט לדוגמה:
```
{"isProgrammer":true,"name":"Yossi","age":30}
Name: Yossi
Age: 30
Is Programmer: true
```

## צלילה לעומק
JSON התפתח בשנות ה-2000 כתסדיר נגיש לעבודה עם נתוני AJAX. ארנטיונות: XML, BSON, YAML. JSON קל לשימוש בזכות ספריות כמו `org.json` או `Jackson` ו`Gson` שמקלות על פענוח והרכבת JSON. הפרטים טכניים הם קידוד UTF-8 ותמיכה במילון (אובייקטים) ומערכים.

## לקרוא גם
- [מדריך לספריית org.json](https://stleary.github.io/JSON-java/)
- [הדרכת Jackson JSON](https://www.baeldung.com/jackson)
- [מבוא ל-Gson](https://www.javatpoint.com/gson-tutorial)
- מפרט התקן הרשמי ל-JSON: [RFC 7159](https://tools.ietf.org/html/rfc7159)
