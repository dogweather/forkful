---
title:                "גירוד מספרים אקראיים"
html_title:           "Haskell: גירוד מספרים אקראיים"
simple_title:         "גירוד מספרים אקראיים"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?
חילוק מספרים אקראיים הוא תהליך של יצירת מספרים שאין לנו תחזית מהימנה לגבי הערך הבא. תכנתים משתמשים בזה לעיתים קרובות, כמו בתוכנות משחקים, באלגוריתמים, באבטחת מסלולים, או בכאלו מסיבות אחרות שדורשות יצירת תוצאות לא צפויות.

## איך לעשות:
```Java
import java.util.Random;

public class Main {
  public static void main(String[] args) {
    Random rand = new Random();
    int rand_int = rand.nextInt(1000);
    System.out.println("Generated Random Number(0-999): "+rand_int);
  }
}

```
התוצאה של הקוד הזה מייצרת מספר אקראי בין 0 ל-999.

## צלילה עמוקה
האיזור בו משתמשים בגנרטורי מספרים אקראיים החל להתפתח בתחילת שנות ה-20. כיום, קיימות שיטות אלגריתמיות רבות ליצירה של המספרים האלה. חשוב לזכור שהמספרים שנוצרים באמצעות ספרייה כמו `java.util.Random` אינם מספרים אקראיים במובנה האמיתי של המילה, אלא "חצי-אקראיים", שכן הם מתחלים ממספר נתון (זרע seed), אך עדיין מספיק טובים לרוב השימושים.

## ראו גם:
1. [Oracle Docs - Random Class in Java](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
2. [Random number generation in Java](https://www.baeldung.com/java-generate-random-long-float-integer-double)