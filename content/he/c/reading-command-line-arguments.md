---
title:    "C: קריאת פרמטרים בשורת הפקודה"
keywords: ["C"]
---

{{< edit_this_page >}}

## למה: 

למי שמתחיל ללמוד תכנות בשפת C, השתמשות בפרמטרי שורת הפקודה עשויה להיראות כמעט כמו קסם. אם אתה יכול לקרוא את הפרמטרים שנמצאים בשורת הפקודה, אתה יכול להשתמש בערך בכמעט כל דבר במחשב.פרמטרים של שורת הפקודה מאפשרים לנו להסתדר עם קלות רבה מאחורי הקלעים, משהו שעשוי להיות מאוד שימושי כאשר אנו מחפשים ליישר את עבודתנו כמתכנתים.

## כיצד לעשות זאת: 

הנה דוגמא פשוטה של שימוש בפרמטרי שורת הפקודה בשפת C:

```c
#include <stdio.h>
 
int main(int argc, char *argv[])
{
    printf("The first argument is: %s\n", argv[0]);
    printf("The second argument is: %s\n", argv[1]);
    printf("The third argument is: %s\n", argv[2]);
 
    return 0;
}
```

כאשר נכתב את הקוד הזה בכפוף להערות ולהדפסות, הפלט שלו צריך להיות זה:

```
$ ./command-line-args hello world
The first argument is: ./command-line-args
The second argument is: hello
The third argument is: world
```

כאן אנחנו משתמשים בשתי משתנים מיוחדים: argc ו-argv. המשתנה argc הוא מספר הפרמטרים בשורת הפקודה, והמשתנה argv הוא מערך של מחרוזות. את הארגומנטים שנמצאים בשורת הפקודה אנו יכולים לגשת אליהם בעזרת סדרת האינדקס של מערך argv. לדוגמה, המשתנה argv[0] מכיל את שם התוכנית, והמשתנה argv[1] מכיל את הארגומנט הראשון שהועבר לתוכנית.

## חקירה עמוקה: 

כמו כל דבר שרלוונטי בתכנות, היכולת לקרוא פרמטרים של שורת הפקודה נעשה הרבה יותר פשוטה כאשר מכירים טוב את מבנה השפה. כאן יש כמה דברים מעניינים