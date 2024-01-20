---
title:                "מציאת אורך המחרוזת"
html_title:           "Elm: מציאת אורך המחרוזת"
simple_title:         "מציאת אורך המחרוזת"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
אורך המחרוזת הוא כמות התווים שבה. בתכנות, אורך המחרוזת רלוונטי לסנכרון, ניתוח ותיקוף התוכן של תווים במחרוזת.

## איך לעשות:
```Java
public class Main {
    public static void main(String[] args) {
        String myString = "Hello, World!";
        int length = myString.length();
        System.out.println("Length of myString: " + length);
    }
}
```
עכשיו, בעזרת הקוד הזה, תוכל לדעת שאורך המחרוזת הוא 13.
```Java
> Output:
> Length of myString: 13
```

## צלילה עמוקה:
- תוך כדי שהשפה Java נוצרה, המשימה הזו התמקדה בקלות השימוש, ולכן הטמיעה את המתודה `length()`.
- פרטי המימוש: כאשר נוצרת מחרוזת ב-Java, אורך המחרוזת מתאחסן כאחד ממאפייניה. זה הופך את פונקציית `length()` למהירה למדי.
- חלופות: אתה יכול להשתמש בלולאה לעבור על כל תו במחרוזת ולספור אותם. זו אפשרות, אך זה פחות יעיל מלהשתמש במתודה `length()`.

## נראה גם:
1. תיעוד Oracle של Java על מחלקת מחרוזת: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html
2. ספר מקוון בחינם על Java: "Thinking in Java" http://www.mindview.net/Books/TIJ/