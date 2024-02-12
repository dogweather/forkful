---
title:                "פיקוח על מספרים אקראיים"
aliases:
- /he/java/generating-random-numbers/
date:                  2024-01-27T20:50:32.362776-07:00
model:                 gpt-4-0125-preview
simple_title:         "פיקוח על מספרים אקראיים"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?

ייצור מספרים אקראיים הוא על כך ליצור סדרות או ערכים בודדים בלתי צפויים בתחום מוגדר. מתכנתים משתמשים בטכניקה זו מגוון סיבות, כולל מדמים, משחקים, יישומים אבטחה, ושיטות לקיום בדיקות משוללת על מנת לבדוק אלגוריתמים תחת תנאים שונים.

## איך:

ב-Java, ניתן ליצור מספרים אקראיים באמצעות השימוש במחלקה `Random` מהחבילה `java.util`, או המחלקות `ThreadLocalRandom` ו'`SecureRandom` לשימושים מסוימים. הדוגמאות הבאות ממחישות איך להשתמש במחלקות אלה.

### באמצעות השימוש במחלקת `Random`
המחלקה `Random` מעניקה דרך ליצור מספרים פסאודו-אקראיים פשוטים.

```Java
import java.util.Random;

public class RandomExample {
    public static void main(String[] args) {
        Random rand = new Random(); // יצירת אובייקט מסוג Random

        int randInt = rand.nextInt(50); // ייצור מספר שלם אקראי מ-0 עד 49
        double randDouble = rand.nextDouble(); // ייצור מספר עשרוני אקראי בין 0.0 ל-1.0
        boolean randBoolean = rand.nextBoolean(); // ייצור בוליאני אקראי
        
        System.out.println("Random Int: " + randInt);
        System.out.println("Random Double: " + randDouble);
        System.out.println("Random Boolean: " + randBoolean);
    }
}
```

### באמצעות השימוש במחלקת `ThreadLocalRandom`
עבור יישומים מתוך, `ThreadLocalRandom` היא יותר יעילה מ-`Random`.

```Java
import java.util.concurrent.ThreadLocalRandom;

public class ThreadLocalRandomExample {
    public static void main(String[] args) {
        int randInt = ThreadLocalRandom.current().nextInt(1, 101); // מ-1 עד 100
        double randDouble = ThreadLocalRandom.current().nextDouble(1.0, 10.0); // מ-1.0 עד 10.0
        
        System.out.println("Random Int: " + randInt);
        System.out.println("Random Double: " + randDouble);
    }
}
```

### באמצעות השימוש במחלקת `SecureRandom`
עבור פעולות קריפטוגרפיות, `SecureRandom` מספקת רמת ביטחון גבוהה יותר.

```Java
import java.security.SecureRandom;

public class SecureRandomExample {
    public static void main(String[] args) {
        SecureRandom secRand = new SecureRandom();
        
        byte[] bytes = new byte[20];
        secRand.nextBytes(bytes); // ממלא את bytes במספרים אקראיים בטוחים
        
        System.out.println("Secure Random Bytes:");
        for (byte b : bytes) {
            System.out.printf("%02x ", b);
        }
    }
}
```

## בחיקה מעמיקה

יצירת מספרים אקראיים זכתה לפיתוחים רבים מימי ההתחלה של המחשוב. מחלקת `Random` של Java משתמשת נוסחה הקונגרואנציאלית ליניארית ליצור מספרים פסאודו-אקראיים, אשר מתנהגים דטרמיניסטית ולא מתאימים ליישומים ברמה הגבוהה של בטחון. זו הבילה להקמת `SecureRandom`, שמשתמשת באלגוריתמים מתקדמים יותר (כמו SHA1PRNG) ליצור מספרים אקראיים חזקים מבחינה קריפטוגרפית.

אבל, ל-`Random` ו-`SecureRandom` יש חסרונות שלהם, כמו זיקוק בביצועים בסביבות מרובות תהליכים. מחלקת `ThreadLocalRandom` הוצגה ב-Java 7 כדי לטפל בבעיה זו על ידי ספקת מחוללים מקומיים לכלל התהליכים של מספרים אקראיים, משפרה באופן משמעותי את הביצועים ביישומים מתוך.

בזמן שמחלקות אלו מספקות את רוב הצרכים, מפתחים עלולים לחקור ספריות נוספות או לפתח פתרונות מותאמים אישית היכן שהדרישות האבטחה והביצועים הם ברמה גבוהה מאד. חשוב לבחור את הדרך הנכונה בהתאם לצרכי האבטחה והביצועים של מקרה השימוש.
