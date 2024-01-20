---
title:                "אינטרפולציה של מחרוזת"
html_title:           "Arduino: אינטרפולציה של מחרוזת"
simple_title:         "אינטרפולציה של מחרוזת"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/interpolating-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
אינטרפולציה של מחרוזת היא תהליך בו נכנסות ערכים משתנים או יתרונות לתוך מחרוזת. מתכנתים משתמשים באינטרפולציה של מחרוזות כדי לבנות מחרוזות דינאמיות ומורכבות באופן יעיל וקריא.

## כיצד נעשה:
אפשר לקבל את התוצאות הדרושות לך על ידי שימוש במתודה `String.format()`. ישנן הרבה דרכים בהן אפשר לבצע את זה, נסו את הדוגמא ותוצאת הקוד למטה:

```Java
int age = 25;
String name = "David";
String intro = String.format("שמי %s ואני בן %d", name, age);
System.out.println(intro);
```

הקוד הזה ידפיס: `"שמי David ואני בן 25"`

## שיעור מעמיק 
אינטרפולציה של מחרוזת היא טכניקה שמרובה שימוש ברוב שפות התכנות המודרניות. בתכנת Java, חלופה לשימוש ב `String.format()` היא פשוט לשרשר את המשתנים והמחרוזות יחד עם '+'. זה מראה את שלושה אפשרויות המוצעות, אך כאשר נדרשת מחרוזת בנויה מרכיבים מרובים, `String.format()` הפך להיות האופציה המועדפת בזכות הקריאות שלה.

## ראה גם:
אם אתה מעוניין ללמוד יותר על אינטרפולציה של מחרוזות, ממליץ לך לקרוא את הכתבות הבאות:
- [Java String.format() - JournalDev](https://www.journaldev.com/360/java-string-format-examples)
- [Java String Interpolation - Baeldung](https://www.baeldung.com/java-string-format)
- [String Interpolation in Java - StackOverflow](https://stackoverflow.com/questions/49357631/java-string-interpolation)