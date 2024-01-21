---
title:                "גילוי מספרים אקראיים"
date:                  2024-01-20T17:49:43.769369-07:00
model:                 gpt-4-1106-preview
simple_title:         "גילוי מספרים אקראיים"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?
יצירת מספרים אקראיים היא התהליך שבו אנו מייצרים מספרים שאינם צפויים לפי סדר מסוים. מתכנתים עושים זאת עבור מטרות שונות, כמו סימולציות, משחקים או בחירות אקראיות.

## איך עושים את זה:
בואו נראה כמה קטעי קוד ב-Java שמייצרים מספרים אקראיים:

```java
import java.util.Random;

public class RandomNumbers {
    public static void main(String[] args) {
        Random rand = new Random();  // מייצר אובייקט Random
        
        int randomInt = rand.nextInt(100); // מספר אקראי בין 0 ל-99
        System.out.println("Random Integer: " + randomInt);
        
        double randomDouble = rand.nextDouble(); // מספר אקראי מסוג double בין 0.0 ל-1.0
        System.out.println("Random Double: " + randomDouble);
    }
}
```

הרצת התוכנית תיתן לך פלט בסגנון:

```
Random Integer: 45
Random Double: 0.792107699314959
```

## צלילה לעומק:
פעם, לפני שגרסאות חדשות של Java הגיעו, היינו משתמשים ב-Math.random() ליצירת מספר אקראי. היום, ל-`Random` יש אחים ואחיות. יש גם את `ThreadLocalRandom` לשימוש במקבילות מסילות, ואת `SecureRandom` לביטחון גבוה יותר.

מבחינת פנימית, `Random` משתמש באלגוריתם שנקרא "Linear congruential generator" (LCG), שהוא פשוט מאוד אבל יחסית חלש מבחינת קריפטוגרפיה. לעומת זאת, `SecureRandom` מאפשר אקראיות חזקה יותר עבור צרכים ביטחוניים, משתמש באלגוריתמים חזקים יותר כמו SHA1PRNG.

כמו כן, "אקראיות" במחשבים היא לא באמת אקראית. אלו מספרים "פסבדו-אקראיים" שנראים אקראיים, אבל בפועל מחושבים על-ידי אלגוריתם מתמטי. 

## גם זה רלוונטי:
- [Oracle's Java Documentation on Random Class](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/Random.html)
- [Baeldung's Guide on Random Number Generation](https://www.baeldung.com/java-generate-random-long-float-integer-double)
- [Stack Overflow Discussion on Random vs SecureRandom](https://stackoverflow.com/questions/11051205/difference-between-java-util-random-and-java-security-securerandom)