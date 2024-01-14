---
title:    "Java: שינוי טקסט לאותיות רישיות"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## למה
בעולם התכנות, ייתכן שתתקלו במצבים בהם תרצו להחליט כיצד תרצו להציג מחרוזת מסוימת. אחת הדרכים הנפוצות היא לכתוב את המחרוזת באותיות גדולות. אתם יכולים לעשות זאת בכמה שורות קוד פשוטות ב-Java ונביא לכם דוגמאות במאמר זה.

## כיצד לכתוב מחרוזת באותיות גדולות

השימוש במתודה `toUpperCase()` יכול לעזור לנו להגדיל את האותיות שבתוך מחרוזת בקלות. כדי להשתמש במתודה זו, נצטרך להפעיל אותה על המחרוזת שאנו רוצים להגדיל ולשמור את התוצאה במשתנה חדש. לדוגמא:

```Java
String word = "hello";
String capitalizedWord = word.toUpperCase();

System.out.println(capitalizedWord);
```

Output:
```
HELLO
```

ניתן להשתמש גם במתודה `toUpperCase()` על משתני מחרוזת ישירות, ללא צורך בשמירת התוצאה במשתנה נוסף. לדוגמא:

```Java
String word = "world";

System.out.println(word.toUpperCase());
```

Output:
```
WORLD
```

## מקורות חומרים

על מנת להעמיק בנושא זה, נוכל להציץ בקישורים הבאים:
- [מדריך על `toUpperCase()` מתוך תיעוד Java](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toUpperCase--)
- [סטאק אוברפלוו - דיון על השימוש במתודה `toUpperCase()`](https://stackoverflow.com/questions/2151175/how-to-capitalize-the-first-character-of-each-word-in-a-string) 

## ראו גם

- [נושא המרת מחרוזת לאותיות גדולות בפייתון](https://www.geeksforgeeks.org/python-string-capitalize/)
- [פתרון תרגיל בשימוש במתודה `toUpperCase()` בלולאה ב-Java](https://www.baeldung.com/java-string-uppercase)