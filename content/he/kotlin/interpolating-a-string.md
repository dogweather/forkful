---
title:                "אינטרפולציה של מחרוזת"
html_title:           "Arduino: אינטרפולציה של מחרוזת"
simple_title:         "אינטרפולציה של מחרוזת"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/interpolating-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
האינטרפולציה של מחרוזת היא ההכנסה של משתנים ישירות לתוך מחרוזת. מתכנתים משתמשים בזה כדי לעשות את הקוד שלהם קריא ומנוסח תמלילית יותר. 

## איך לעשות את זה?
הבאת משתנה לתוך מחרוזת ב-Kotlin היא פשוטה ויש לה שני אופנים:

```Kotlin
    // בצורה מרוחקת
    val name = "Ariel"
    println("שלום, " + name + "!") // "שלום, Ariel!"

    // באמצעות אינטרפולציה
    val greeting = "שלום"
    println("$greeting, $name!") // "שלום, Ariel!"
```

## צלילה עמוקה
המטרה של אינטרפולציה של מחרוזות היא שיפור נראות הקוד והפשטתו. מבחינה היסטורית, האינטרפולציה התפתחה בשפת Perl, אך התפתחה ושופרה במהלך השנים והגיעה אל Kotlin.

מכאן, ישנן דרך אלטרנטיבית לאינטרפולציה של מחרוזות שלא נדרשת בשפת Kotlin, אך משמשת כדוגמה לדרכים אחרות: השימוש בפונקציה `format` של מחרוזת (כמו בשפת Java).

```Kotlin
    val msg = "שלום"
    val name = "Ariel"
    val formattedStr = String.format("%s, %s!", msg, name) // "שלום, Ariel!"
    println(formattedStr)
```

כאן, צריך לשים לב לסדר המשתנים שניתנים לפונקציה.

## בהמשך
למדענים ולסטודנטים שאוהבים לחקור יותר, הנה קודם מאמרים וקישורים שיכולים לעזור:
- [הצגת מחרוזת בקוטלין](https://kotlinlang.org/docs/idioms.html#string-interpolation)