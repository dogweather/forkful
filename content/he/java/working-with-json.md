---
title:                "לעבוד עם json"
html_title:           "Java: לעבוד עם json"
simple_title:         "לעבוד עם json"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/working-with-json.md"
---

{{< edit_this_page >}}

# למה:
JSON היא שפת תכנות נפוצה כידוף צורות מידע במיקוד על מדינות ומיצרות, וכן על מציאות."

## איך לעבוד עם JSON:
ראשית, ניתן להתקין ספריית JSON לפרויקט שלנו כדי לאפשר עבודה עם פעולות JSON. לאחר מכן, אנו יכולים ליצור אובייקט JSON חדש ולהכניס לו מידע באמצעות שדות וערכים מתאימים. לבסוף, ניתן להשתמש בפקודת toString () כדי להמיר את האובייקט שנוצר למחרוזת JSON.

```Java
// התקנת ספריית JSON לפרויקט
import org.json.JSONObject;

// יצירת אובייקט JSON חדש
JSONObject json = new JSONObject();

// הוספת שדות וערכים לאובייקט
json.put("name", "John");
json.put("age", 30);
json.put("isMarried", true);

// המרת האובייקט למחרוזת JSON והדפסת התוצאה
System.out.println(json.toString());
```

> פלט:
> {"name":"John","age":30,"isMarried":true}

## Deep Dive:
כאשר אנחנו מתכנתים בג'אווה, עבודה עם JSON יכולה להיות חלק חשוב מהתהליך. אם אנחנו משתמשים בקבצי הגדרות כדי לאחסן נתונים, שימוש בפורמט JSON יכול להקל עלינו מאד בטיפול בנתונים האלה. כמו כן, עם התקדמות התכנות ניתן למצוא שימושים רבים נוספים לפורמט הזה כמו לפתוח ולסגל נתונים מכמה מקומות שונים בזמן ריצת הקוד.

# ראה גם:
- [ספריית JSON בג'אווה](https://mvnrepository.com/artifact/org.json/json)
- [מדריך לעבודה עם פורמט JSON](https://www.tutorialspoint.com/json/index.htm)
- [מידע נוסף על JSON](https://www.json.org/json-he.html)