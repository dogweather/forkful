---
title:                "טיפול בשגיאות"
aliases: - /he/java/handling-errors.md
date:                  2024-01-26T00:54:10.636391-07:00
model:                 gpt-4-1106-preview
simple_title:         "טיפול בשגיאות"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/handling-errors.md"
---

{{< edit_this_page >}}

## מה ולמה?

טיפול בשגיאות משמעו כתיבת קוד שמתחשב באפשרות שדברים ילכו לא כשורה ומתמודד עמם. מתכנתים עושים זאת במטרה ליצור תוכנה עמידה, שתמנע קריסות והתנהגות מוזרה.

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
