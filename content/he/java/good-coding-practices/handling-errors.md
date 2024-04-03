---
date: 2024-01-26 00:54:10.636391-07:00
description: "\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA\
  \ \u05DE\u05E9\u05DE\u05E2\u05D5 \u05DB\u05EA\u05D9\u05D1\u05EA \u05E7\u05D5\u05D3\
  \ \u05E9\u05DE\u05EA\u05D7\u05E9\u05D1 \u05D1\u05D0\u05E4\u05E9\u05E8\u05D5\u05EA\
  \ \u05E9\u05D3\u05D1\u05E8\u05D9\u05DD \u05D9\u05DC\u05DB\u05D5 \u05DC\u05D0 \u05DB\
  \u05E9\u05D5\u05E8\u05D4 \u05D5\u05DE\u05EA\u05DE\u05D5\u05D3\u05D3 \u05E2\u05DE\
  \u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD\
  \ \u05D6\u05D0\u05EA \u05D1\u05DE\u05D8\u05E8\u05D4 \u05DC\u05D9\u05E6\u05D5\u05E8\
  \ \u05EA\u05D5\u05DB\u05E0\u05D4 \u05E2\u05DE\u05D9\u05D3\u05D4, \u05E9\u05EA\u05DE\
  \u05E0\u05E2 \u05E7\u05E8\u05D9\u05E1\u05D5\u05EA \u05D5\u05D4\u05EA\u05E0\u05D4\
  \u05D2\u05D5\u05EA \u05DE\u05D5\u05D6\u05E8\u05D4."
lastmod: '2024-03-13T22:44:39.143384-06:00'
model: gpt-4-1106-preview
summary: "\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA\
  \ \u05DE\u05E9\u05DE\u05E2\u05D5 \u05DB\u05EA\u05D9\u05D1\u05EA \u05E7\u05D5\u05D3\
  \ \u05E9\u05DE\u05EA\u05D7\u05E9\u05D1 \u05D1\u05D0\u05E4\u05E9\u05E8\u05D5\u05EA\
  \ \u05E9\u05D3\u05D1\u05E8\u05D9\u05DD \u05D9\u05DC\u05DB\u05D5 \u05DC\u05D0 \u05DB\
  \u05E9\u05D5\u05E8\u05D4 \u05D5\u05DE\u05EA\u05DE\u05D5\u05D3\u05D3 \u05E2\u05DE\
  \u05DD."
title: "\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA"
weight: 16
---

## איך לעשות:
ג'אווה משתמשת בחריגים (exceptions) לטיפול בשגיאות. אתה מקיף קוד בעל סיכון לאיונים בבלוק `try` ותופס חריגים באמצעות `catch`. הנה דוגמה פשוטה:

```java
public class ErrorHandlingExample {
    public static void main(String[] args) {
        try {
            int result = divide(10, 0);
            System.out.println("התוצאה היא: " + result);
        } catch (ArithmeticException e) {
            System.out.println("אופס, לא ניתן לחלק באפס!");
        }
    }

    private static int divide(int molecule, int denominator) {
        return molecule / denominator;
    }
}
```

פלט:
```
אופס, לא ניתן לחלק באפס!
```

## צלילה עמוקה
טיפול בשגיאות בג'אווה השתנה לאורך השנים. בתחילת הדרך לא היו חריגים; המתכנתים בדקו קודי שגיאה. לאחר מכן ג'אווה הציגה בלוקי try-catch, שמאפשרים טיפול יותר אלגנטי בשגיאות.

אלטרנטיבות ל`try-catch` המסורתי כוללות את `try-with-resources` לסגירת משאבים אוטומטית וקוד נקי יותר, שהוצג בג'אווה 7.

פרטי המימוש חשובים. לדוגמה, לתפוס `Exception` או `Throwable` בדרך כלל נחשב לתרגול רע. זה רחב מדי ויכול להסתיר באגים שאולי אתה לא מודע להם. עדיף להתמקד בחריגים ספציפיים.

## ראה גם
- המדריכים הרשמיים של אורקל לג'אווה על חריגים: [https://docs.oracle.com/javase/tutorial/essential/exceptions/](https://docs.oracle.com/javase/tutorial/essential/exceptions/)
- תיעוד ההצהרה של ג'אווה `try-with-resources`: [https://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html](https://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html)
- Effective Java מאת ג'ושוע בלוך, למיטב התרגולים בנושא חריגים.
