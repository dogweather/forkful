---
title:                "חיפוש והחלפת טקסטים."
html_title:           "Java: חיפוש והחלפת טקסטים."
simple_title:         "חיפוש והחלפת טקסטים."
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## למה

כחלק מתהליך התכנות, חלק עיקרי של עבודה הוא למצוא ולהחליף טקסט בקוד. זה עשוי להיות בשביל לתקן תקלות, לשנות פונקציות או לשנות את התוכן המוצג למשתמשים. מצא והחלף של טקסט היא כלי חשוב ומועיל שסוף סוף יכול לחסוך לנו זמן ומאמץ יקרים במהלך תהליך התכנות.

## איך לעשות זאת

אם תרצו לחפש ולהחליף טקסט בקוד Java, ישנן מספר דרכים שאולי תמצאו שימושיות. תוכלו להשתמש בפונקציות המובנות של Java, כגון `replaceAll()`, או להשתמש בספריות חיצוניות כמו Regex במידת הצורך. הנה כמה דוגמאות לחיפוש והחלפת טקסט בקוד Java:

```
// חיפוש והחלפת טקסט באמצעות פונקציות המובנות
String text = "Java היא שפת תכנות יפה ומוצלחת";
String newText = text.replaceAll("כחלק", "כחלק מה");

// חיפוש והחלפת טקסט באמצעות Regex
import java.util.regex.Matcher;
import java.util.regex.Pattern;

// יצירת אובייקט Regex
Pattern pattern = Pattern.compile("שפת (.*) מוצלחת");
Matcher matcher = pattern.matcher(text);

// מציאת התאמה והחלפת הטקסט
if (matcher.find()) {
    newText = matcher.replaceAll("שפת $1 מהמוצלחות");
}
```

התוכניות `replaceAll()` ו- `replaceAll()` מחזירות את הטקסט ששונה. הן מקבלות כפרמטר את הטקסט שאנחנו מחפשים ואת הטקסט החדש שאנחנו רוצים להחליף בו. בעבודה עם Regex, נצטרך ליצור תבנית רגולרית ולהשתמש במתודות המתאימות עבור תיבת טקסט וגדולות.

## חפירה עמוקה

החיפוש והחלפת טקסט בקוד Java מתב