---
title:                "Kotlin: קריאת פרמטרים של שורת פקודה"
simple_title:         "קריאת פרמטרים של שורת פקודה"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

למה: רק 1-2 משפטים שמסבירים *למה* מישהו ירצה לקרוא על פרמטרי קו הפקודה.

בכתיבה של קוד ואקודים לעובדות ונתונים בתוך בלוקי קוד "```Kotlin ... ```" 
כיצד לה: 

```Kotlin
fun main(args: Array<String>) {
    // קוד לקריאת פרמטרים מקו הפקודה
    // שימוש בערכים שישמורו במשתנה arguments
    // עבור כל פרמטר במערך args
    for (argument in args) {
        println(argument)
    }
}
```

פלט (output):

```Bash
> kotlin MainClass argument1 argument2 argument3
 
argument1
argument2
argument3
```

עומק: פרמטרי קו הפקודה הם מידע משמעותי שמסופק כקלט לתוכנית. בכתיבה של תוכניות בקונסול, פרמטרי קו הפקודה חשובים לצורך עיבוד של קלטים שונים וכן להתאמת התוכנית לצרכי המשתמש. כתבות יכולות להיות מאוד מעניינות לכתיבת קני ועובדים בצבי התכנות של פרטי קו ההפקות האישיים שיש להם.

ראו גם:
- [קוד קיים לקריאת פרמטרי קו הפקודה עם Kotlin](https://kotlinexpertise.com/command-line-arguments-kotlin/)
- [כיצד להגביל את כמות פרמטרי קו הפקודה בכתיבת תוכניות עם Kotlin](https://stackoverflow.com/questions/52233833/limit-the-number-of-command-line-arguments-in-kotlin)
- [פתרון לבעיית הכפייה של פרמטרי קו הפקודה עם Kotlin](https://blog.mariuszs.pl/2018/04/01/fix-excuse-to-trust-kotlin.html)