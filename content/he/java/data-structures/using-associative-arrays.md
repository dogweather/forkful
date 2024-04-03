---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:27.913313-07:00
description: "\u05D1\u05D2'\u05D0\u05D5\u05D5\u05D4, \u05DE\u05E2\u05E8\u05DB\u05D9\
  \u05DD \u05D0\u05E1\u05D5\u05E6\u05D9\u05D0\u05D8\u05D9\u05D1\u05D9\u05D9\u05DD\
  , \u05D0\u05D5 \u05DE\u05E4\u05D5\u05EA, \u05DE\u05D0\u05E4\u05E9\u05E8\u05D9\u05DD\
  \ \u05DC\u05DA \u05DC\u05D0\u05D7\u05E1\u05DF \u05D6\u05D5\u05D2\u05D5\u05EA \u05DE\
  \u05E4\u05EA\u05D7-\u05E2\u05E8\u05DA \u05DC\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\
  \u05DC\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\
  \ \u05D1\u05E6\u05D5\u05E8\u05D4 \u05D9\u05E2\u05D9\u05DC\u05D4. \u05DE\u05EA\u05DB\
  \u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D4\
  \u05DD \u05DC\u05DE\u05E9\u05D9\u05DE\u05D5\u05EA \u05DB\u05DE\u05D5 \u05E1\u05E4\
  \u05D9\u05E8\u05EA \u05DE\u05D5\u05E4\u05E2\u05D9\u05DD \u05E9\u05DC\u2026"
lastmod: '2024-03-13T22:44:39.118699-06:00'
model: gpt-4-0125-preview
summary: "\u05D1\u05D2'\u05D0\u05D5\u05D5\u05D4, \u05DE\u05E2\u05E8\u05DB\u05D9\u05DD\
  \ \u05D0\u05E1\u05D5\u05E6\u05D9\u05D0\u05D8\u05D9\u05D1\u05D9\u05D9\u05DD, \u05D0\
  \u05D5 \u05DE\u05E4\u05D5\u05EA, \u05DE\u05D0\u05E4\u05E9\u05E8\u05D9\u05DD \u05DC\
  \u05DA \u05DC\u05D0\u05D7\u05E1\u05DF \u05D6\u05D5\u05D2\u05D5\u05EA \u05DE\u05E4\
  \u05EA\u05D7-\u05E2\u05E8\u05DA \u05DC\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05DC\
  \u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D1\
  \u05E6\u05D5\u05E8\u05D4 \u05D9\u05E2\u05D9\u05DC\u05D4."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E2\u05E8\u05DB\u05D9\u05DD\
  \ \u05D0\u05E1\u05D5\u05E6\u05D9\u05D0\u05D8\u05D9\u05D1\u05D9\u05D9\u05DD"
weight: 15
---

## מה ולמה?

בג'אווה, מערכים אסוציאטיביים, או מפות, מאפשרים לך לאחסן זוגות מפתח-ערך לחיפוש ולטיפול בנתונים בצורה יעילה. מתכנתים משתמשים בהם למשימות כמו ספירת מופעים של פריטים או מיפוי משתמשים להרשאות שלהם משום שהם מציעים גישה ועדכונים מהירים.

## איך ל:

ג'אווה אינה מכילה מערכים אסוציאטיביים מובנים כמו כמה שפות אחרות, אך היא מספקת את הממשק `Map` וכיתות כמו `HashMap` ו-`TreeMap` למילוי התפקיד הזה. הנה איך להשתמש ב-`HashMap`:

```Java
import java.util.HashMap;
import java.util.Map;

public class LearnMaps {
    public static void main(String[] args) {
        // יצירת HashMap
        Map<String, Integer> ageOfFriends = new HashMap<>();
        
        // הוספת אלמנטים
        ageOfFriends.put("Alice", 24);
        ageOfFriends.put("Bob", 30);
        ageOfFriends.put("Charlie", 28);

        // גישה לאלמנטים
        System.out.println("גילו של אליס: " + ageOfFriends.get("Alice"));
        
        // טיפול במפתחות שאינם קיימים
        System.out.println("גיל של מישהו שלא נמצא במפה: " + ageOfFriends.getOrDefault("Dan", -1));

        // איטרציה על פני אלמנטים
        for (Map.Entry<String, Integer> entry : ageOfFriends.entrySet()) {
            System.out.println(entry.getKey() + " בן " + entry.getValue() + " שנים.");
        }
    }
}
```

פלט לדוגמה:

```
גילו של אליס: 24
גיל של מישהו שלא נמצא במפה: -1
אליס בת 24 שנים.
בוב בן 30 שנים.
צ'ארלי בן 28 שנים.
```

`HashMap` הוא רק אחת המימושים. אם המפתחות שלך ייחודיים ואתה צריך אותם ממוינים, שקול את `TreeMap`. עבור מפה ששומרת על סדר ההכנסה, `LinkedHashMap` הוא החבר שלך.

## צלילה עמוקה

מפות בג'אווה הן חלק ממסגרת האוספים, שהוצגה ב-JDK 1.2, אך חוותה שיפורים משמעותיים לאורך השנים, כולל הצגת המתודה `forEach` בג'אווה 8 לאיטרציה קלה יותר על פני הרשומות. בחירת המימוש של המפה (`HashMap`, `LinkedHashMap`, `TreeMap`) צריכה להתבצע לפי הצרכים הספציפיים שלך בקשר לסדר ולביצועים. לדוגמה, `HashMap` מציעה ביצועי זמן בסדר O(1) לפעולות הבסיסיות (get ו-put), בהנחה שפונקציית הגיבוב מפזרת את האלמנטים באופן נאות בין הדליים. עם זאת, אם אתה זקוק למיון לפי סדר טבעי או משוואים מותאמים אישית, `TreeMap` היא האופציה המועדפת, המספקת זמן בסדר O(log n) להכנסה ולחיפוש.

לפני שהוצג ה`Map`, פעמים רבות מערכים אסוציאטיביים היו ממומשים באמצעות שני מערכים מקבילים (אחד למפתחות ואחד לערכים) או מבני נתונים מותאמים אישית עם פחות יעילות. אפשרויות חלופיות נוכחיות ל`Map` ולמימושים שלו עשויות לכלול ספריות צד שלישי המציעות מפות מתמחות, כגון מפות דו-כיווניות (BiMap בספריית Guava של Google) למקרים בהם אתה צריך למצוא מפתח לפי הערך שלו בצורה יעילה. עם זאת, לרוב המקרים בג'אווה, מפות הספרייה הסטנדרטית עמידות וגמישות מספיק כדי להתמודד עם המשימה.
