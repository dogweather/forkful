---
title:                "הפיכת מחרוזת לאותיות גדולות"
html_title:           "Java: הפיכת מחרוזת לאותיות גדולות"
simple_title:         "הפיכת מחרוזת לאותיות גדולות"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
"הגדלת אותיות במחרוזת" הוא להביא את האות הראשונה במילה למצב של אות גדולה, או בראשית המחרוזת. פוגרמרים משתמשים בכך לשיפור נראות הטקסט, לדוג' בכותרות או שמות משתמש.

## איך לעשות:
הנה שיטה בסיסית ב-Java להגדלת אותיות במחרוזת:

```Java 
String str = "hello world";
String capitalized = str.substring(0,1).toUpperCase() + str.substring(1).toLowerCase();
System.out.println(capitalized);   
```

ההדפסה תחזיר: "Hello world"

## צלילה מעמיקה
זיכרון ביסטורי: חשוב לזכור כי השיטה שהצגנו היא בסיסית ולא מתאימה לשפות שלא משתמשות באלפאבית הלטיני. במקרים אלו, תהיה צורך בשיטות מתקדמות יותר.

אלטרנטיבות: Java מציעה כלים נוספים, כמו `WordUtils.capitalizeFully(str)` מספריית Apache Commons.

פרטי המימוש: השיטה שהצגנו משנה את האות הראשונה בכל מילה לאות גדולה ושאר האותיות תהיינה קטנות. אם זה לא מה שאתה רוצה, שנה את הקוד שלך בהתאם.

## ראה גם
Java String Documentation: https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html

Apache Commons WordUtils: https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/text/WordUtils.html