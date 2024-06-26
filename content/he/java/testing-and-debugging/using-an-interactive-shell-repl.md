---
date: 2024-01-26 04:15:52.278864-07:00
description: "\u05D0\u05D9\u05DA \u05DC: \u05DC\u05D4\u05EA\u05D7\u05D9\u05DC REPL\
  \ \u05D1-Java \u05E4\u05E9\u05D5\u05D8 \u05E2\u05DD \u05D4\u05DB\u05DC\u05D9 `jshell`\
  \ \u05E9\u05D4\u05D5\u05E6\u05D2 \u05D1-Java 9. \u05D4\u05E0\u05D4 \u05D0\u05D9\u05DA\
  \ \u05DC\u05D4\u05E9\u05D9\u05D2 \u05D0\u05D5\u05EA\u05D5 \u05D5\u05DC\u05D4\u05EA\
  \u05D7\u05D9\u05DC \u05E1\u05E9\u05DF \u05D1\u05E1\u05D9\u05E1\u05D9."
lastmod: '2024-03-13T22:44:39.133549-06:00'
model: gpt-4-0125-preview
summary: "\u05DC\u05D4\u05EA\u05D7\u05D9\u05DC REPL \u05D1-Java \u05E4\u05E9\u05D5\
  \u05D8 \u05E2\u05DD \u05D4\u05DB\u05DC\u05D9 `jshell` \u05E9\u05D4\u05D5\u05E6\u05D2\
  \ \u05D1-Java 9."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E2\u05D8\u05E4\u05EA \u05D0\
  \u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\u05D8\u05D9\u05D1\u05D9\u05EA (REPL)"
weight: 34
---

## איך ל:
להתחיל REPL ב-Java פשוט עם הכלי `jshell` שהוצג ב-Java 9. הנה איך להשיג אותו ולהתחיל סשן בסיסי:

```Java
jshell> int sum(int a, int b) {
   ...> return a + b;
   ...> }
|  נוצרה הפונקציה sum(int,int)

jshell> sum(5, 7)
$1 ==> 12
```

יציאה בכל זמן עם `/exit`.

```Java
jshell> /exit
|  שלום
```

## צלילה עמוקה
לפני `jshell`, למתכנתי Java לא היה REPL רשמי, בניגוד למתכנתי Python או Ruby. הם השתמשו ב-IDEs או כתבו תוכניות מלאות גם למשימות טריוויאליות. `jshell` היה משנה משחק החל מ-Java 9, וסגר את הפער הזה.

אלטרנטיבות כוללות מהדרים אונליין או תוספי IDE, אך הם לא מתחרים במיידיות של `jshell`. לגבי המנגנונים הפנימיים, `jshell` משתמש ב-Java Compiler API לביצוע קטעי קוד, דבר שמרשים למדי. זה יותר מרק מגרש משחקים - הוא יכול לייבא ספריות, להגדיר מחלקות ועוד. זה הופך אותו לכלי חזק ליצירת פרוטוטיפים.

## ראו גם
- [מדריך למשתמש של JShell](https://docs.oracle.com/javase/9/jshell/introduction-jshell.htm)
- [המדריך לכלים במהדורה הסטנדרטית של פלטפורמת Java](https://docs.oracle.com/javase/9/tools/tools-and-command-reference.htm#JSWOR719)
- [Java Compiler API](https://docs.oracle.com/javase/9/docs/api/javax/tools/JavaCompiler.html)
