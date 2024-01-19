---
title:                "חיבור מחרוזות"
html_title:           "C++: חיבור מחרוזות"
simple_title:         "חיבור מחרוזות"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## מה זה ולמה?: (What & Why?)
הצירוף של מחרוזות הוא להכניס הרבה מידע בשורה אחת באופן מוסרד. מתכנתים משתמשים בזה להוסיף טטסט, לבנות מחרוזת שמייצגת מידע מרובה או לשים יחד נתונים ממקורות שונים.

## איך לעשות זאת: (How to)
הנה משל לכמה דרכים לעשות זאת ב-Java.

מנותח '+':
```Java
String firstName = "John";
String lastName = "Doe";
String fullName = firstName + " " + lastName;
System.out.println(fullName); // Prints: John Doe
```
באמצעות StringBuilder:
```Java
StringBuilder sb = new StringBuilder();
sb.append("John");
sb.append(" ");
sb.append("Doe");
System.out.println(sb.toString()); // Prints: John Doe
```
## התרתעות (Deep Dive)
בהקשר ההיסטורי, המנותח '+' הוא אופציה פופולרית, אך יעיל רק למספר קטן של מחרוזות. עבור צירוף מחרוזת מרובה, StringBuilder הוא אפשרות יותר יעילה מבחינת הביצועים.

בנוסף ל'+' ו-StringBuilder, קיימות אפשרויות נוספות כמו String.format בעבודה גם עם מחרוזת מורכבת.

בהנחה שאתה משתמש ב-Java 8 או גרסה חדשה יותר, ניתן לדעת שהצירוף של מחרוזות מתבצע נכונה במדרך דינמי של JVM בשמו 'indify string concatenation'.

## לראות גם: (See Also)
- [Documentations on String class in Java](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Efficient String Concatenation in Java](https://dzone.com/articles/string-concatenation-performacne-improvement-in-ja)
- [Oracle tutorial on string concatenation](https://docs.oracle.com/javase/tutorial/java/data/buffers.html)