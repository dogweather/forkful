---
title:                "חיפוש והחלפת טקסט"
html_title:           "Elm: חיפוש והחלפת טקסט"
simple_title:         "חיפוש והחלפת טקסט"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## מה ולמה?
חיפוש והחלפת טקסט הם פעולות שנעשים במקרא למציאת מחרוזות מסוימות והחלפתם באחרות. מתכנתים עושים את זה לשם שינוי פרטים במקראות, או לשקפר את הקריאות.

## שיטה:
פה נמחיש את החיפוש וההחלפה באמצעות מחלקת String בג'אווה:

```Java
public class Main {
  public static void main(String[] args) {
    String line = "I love to code in Java.";
    String newLine = line.replace("Java", "JavaScript");
    System.out.println(newLine);
  }
}
```
פלט:

```"I love to code in JavaScript."```

## טבילה עמוקה
חיפוש והחלפה של טקסט התפתח כעזרה עבור מתכנתים לערוך מקראות. אפשרות אחת אחרת היא שימוש בביטויים רגילים, שמאפשרים התאמה גם למקרים יותר מורכבים. בנוגע לג'אווה, המתודה replace של מחלקת String מבצעת את ההחלפה בכל המחרוזת, ולא מתחילה במופע הראשון בלבד.

## ראו גם
- [Java String replace() - Javatpoint](https://www.javatpoint.com/java-string-replace)
- [Java - Regular Expressions - Tutorialspoint](https://www.tutorialspoint.com/java/java_regular_expressions.htm)