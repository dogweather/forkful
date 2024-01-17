---
title:                "יצירת מספרים אקראיים"
html_title:           "Java: יצירת מספרים אקראיים"
simple_title:         "יצירת מספרים אקראיים"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

# מה זה ולמה?

הכנת מספרים אקראיים הוא פעולה נפוצה בתחום התכנות, שמטרתה ליצור מספרים באופן בלתי צפוי. תכנית כזו יכולה לשמש למגוון מטרות, כגון בדיקת תכונות של תוכנית, יצירת מפה של נתונים ועוד.

# איך לעשות:

להלן דוגמאות של כיצד ליצור מספרים אקראיים בשפת ג'אווה:

```java
// ייבוא המודול המתאים
import java.util.Random;

// יצירת אובייקט מסוג Random
Random random = new Random();

// יצירת מספר אקראי בין 1 ל-10
int randomNumber = random.nextInt(10) + 1;
System.out.println("מספר אקראי בין 1 ל-10: " + randomNumber);

// יצירת מספר אקראי מסוג double בין 0 ל-1
double randomDouble = random.nextDouble();
System.out.println("מספר אקראי מסוג double בין 0 ל-1: " + randomDouble);
```

# טיול עמוק:

הכנת מספרים אקראיים נמצאת בשימוש רב בתחום התכנות והמחשבים. בעבר, היתה תהליך מאוד מתוחכם ליצור מספרים אקראיים במחשב. כיום, ישנם כלים נוחים ופשוטים המאפשרים למתכנתים ליצור מספרים אקראיים בקלות.

ישנם גם אפשרויות אחרות ליצירת מספרים אקראיים, כגון חומרת המחשב או רעש שלו. תוצאות של מספרים אקראיים יכולות להשתנות בין הרצת התכנית, ולכן חשוב לבחון את הסביבה שבה מתבצעת תהליך כזה.

# ראה גם:

- מידע נוסף על מספרים אקראיים ואלגוריתמים ליצירתם: https://he.wikipedia.org/wiki/%D7%9E%D7%A1%D7%A4%D7%A8_%D7%90%D7%A7%D7%A8%D7%90%D7%99
- תיעוד מלא על מחלקת Random בשפת ג'אווה: https://docs.oracle.com/javase/8/docs/api/java/util/Random.html