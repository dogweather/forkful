---
title:                "שימוש במעטפת אינטראקטיבית (REPL)"
date:                  2024-01-26T04:15:52.278864-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש במעטפת אינטראקטיבית (REPL)"

category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## מה ולמה?
REPL (לולאת קריאה-חישוב-הדפסה) היא מעטפת אינטראקטיבית שמעבדת קלטים יחידים מהמשתמש, מבצעת קוד ומחזירה את התוצאה. מתכנתים משתמשים בה לניסויים מהירים, ניפוי שגיאות או למידה, מכיוון שהיא מאפשרת משוב מיידי ואיטרציה.

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
