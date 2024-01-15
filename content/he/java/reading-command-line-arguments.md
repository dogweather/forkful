---
title:                "קריאת ארגומנטים מהשורת פקודה"
html_title:           "Java: קריאת ארגומנטים מהשורת פקודה"
simple_title:         "קריאת ארגומנטים מהשורת פקודה"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

##
למה: בכדי ליישם את הקוד שלכם בצורה יעילה ומאפשרת מידע נלווה לתוכנית שלכם.

איך לעשות זאת: הכי פשוט זה להתחיל לקרוא פרמטרים של שורת הפקודה בעזרת המתודה "getArgs()" ולהשתמש במערך הפלט שנוצר כדי לקבל את הפרמטרים הקולטים.

```Java
public class CommandLineArguments {
    public static void main(String[] args) {
        // print out each argument
        for (String arg : args) {
            System.out.println(arg);
        }
    }
}
```

כאשר כיתבתי בצורה זו "Java CommandLineArguments Hello World" יתקבל הפלט הבא:
"Hello World"

מכאן תוכלו לפתח את התוכנית שלכם ולהשתמש בפרמטרים שהכנסתם כדי למקסם את היכולות של התוכנית שלכם.

הצצה עמוקה: כאשר אתם מטפלים בפרמטרים שהתקבלו משורת הפקודה, יש לקחת בחשבון כי הם יכולים להיות בכל צורה וסדר שתרצו כדי להתאים לצרכי התוכנית שלכם. אתם יכולים להשתמש בתנאים ולבדוק את כל האפשרויות האפשריות על מנת לקבל את המידע שאתם רוצים מהפרמטרים הנתונים.

הקוד הבא מדגים איך ניתן להשתמש במתודה "startsWith()" כדי לבדוק אם הפרמטר הנתון מתחיל עם מילה מסוימת ולאחר מכן לבדוק עם "substring()" כמה מילים יש לאחר המילה הראשונה.

```Java
public class CheckArgs {
    public static void main(String[] args) {
        for (String arg : args) {
            if (arg.startsWith("Hello")) { // if the argument starts with "Hello"
                String rest = arg.substring(5); // get the rest of the word (after "Hello")
                if (rest.length() > 5) { // if the rest of the word is longer than 5 characters
                    System.out.println(rest); // print it out
                }
            }
        }
    }
}
```

לדוגמה, כאשר נכניס את הפרמטר "HelloWorld Java", יתקבל הפלט הבא:
"World Java"

רואים כאן שאפשר להשתמש גם במתודות נוספות בשילוב