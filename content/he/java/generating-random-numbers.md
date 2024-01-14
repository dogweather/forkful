---
title:    "Java: יצירת מספרים אקראיים"
keywords: ["Java"]
---

{{< edit_this_page >}}

## למה:

יצירת מספרים אקראיים היא כלי חשוב בתחום התכנות המסייע לפתור בעיות שונות. השימוש במספרים אקראיים מאפשר לנו ליצור כמויות גדולות של נתונים ללא צורך בהתעסקות מייגעת עם כל נתון בנפרד. בכתבה זו נפתח את עולם המספרים האקראיים בשפת ג'אווה ונסביר היכן וכיצד ניתן להשתמש בהם.

## כיצד להשתמש:

ניתן ליצור מספרים אקראיים בשפת ג'אווה בעזרת הספריה המובנית "java.util". נבחר בצורה אקראית בין מספרים מתנתקים כגון תאריכים ומספרים שלמים. בתכנות הבא נדגים כיצד ניתן ליצור מספרים אקראיים ואיך זה נראה בשפת ג'אווה.

```java
import java.util.Random;

public class RandomNumbers {

  public static void main(String[] args) {
    // יצירת אובייקט מסוג Random
    Random random = new Random();

    // הדפסת מספר אקראי בין 0 ל-100
    System.out.println(random.nextInt(100));
  }
}
```

פלט:

```
59
```

## חקירה מעמיקה:

השתמשנו בפקודה `nextInt` כדי להיות מסוגלים ליצור מספרים אקראיים בתיחום כלשהו שנרצה. אם נרצה לקבל מספר אקראי בין 10 ל-20 נצטרך רק לשנות את הערך השני של הפקודה ל-10 ואת הערך הראשון ל-20. ניתן גם להשתמש בפקודות נוספות כגון `nextDouble` ליצירת מספרים אקראיים ממש בדיוק כפי שתרצו.

## ראו גם:

- [Java Random number generation](https://www.geeksforgeeks.org/generating-random-numbers-in-java/)
- [Java Random class documentation](https://docs.oracle.com/javase/7/docs/api/java/util/Random.html)
- [Java Random numbers tutorial](https://www.javatpoint.com/java-random)