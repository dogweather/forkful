---
title:                "מחיקת תווים התואמים לתבנית"
html_title:           "Java: מחיקת תווים התואמים לתבנית"
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

כתיבת תכנות בשפת ג'אווה (הגרסה הנוכחית) מתאימה לקריאה בעברית

## מה זה ולמה?
מחיקת תווים המתאימים לתבנית היא פעולה שמבוצעת על טקסט ומוכרת למתכנתים בשם "Regex". זהו תהליך המאפשר למשתמשים למחוק או לשנות תווים ספציפיים בטקסט בקלות וביעילות.

## איך לעשות זאת?
ניתן להשתמש בכמה שורות קוד פשוטות כדי למחוק תווים המתאימים לתבנית בטקסט.

```java
String input = "זהו טקסט עם תווים שאנחנו רוצים למחוק";
String output = input.replaceAll("[א-ת]", "");

System.out.println(output);
```

כאן אנו משתמשים בפקודת הפעלת `replaceAll` כדי להחליף את תווים האותיות העבריות בריק. פשוט וממוקד!

## יזרוע מעמיקה
מחיקת תווים המתאימים לתבנית היא פעולה נפוצה שמשמשת בתחום המחשבים כבר מזמן רב. מתכנני ישיבות ומפתחי תוכנה משתמשים בפונקציות Regex כדי לאלץ תווים בטקסט להתאים לתבנית מסוימת. יחד עם זאת, כמו בכל פעולה אחרת, ישנן פתרונות אחרים כגון פקודות נוספות בשפות תכנות אחרות, אך Regex נותן כלים נפלאים לשיפור הביצועים והאפקטיביות.

## ראו גם
* מידע נוסף על פקודת הפעלת `replaceAll` במדריך Java: https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#replaceAll(java.lang.String,%20java.lang.String)
* יישום נוסף להשתמש בפקודות Regex בשפת ג'אווה: https://www.javatpoint.com/java-regex