---
title:                "אינטרפולציה של מחרוזת"
html_title:           "Arduino: אינטרפולציה של מחרוזת"
simple_title:         "אינטרפולציה של מחרוזת"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/interpolating-a-string.md"
---

{{< edit_this_page >}}

## מה זה ולמה?:

הכנסת משתנים לתוך מחרוזת, או "אינטרפולציה של מחרוזת", היא מערכת שמאפשרת לנו להוסיף נתונים דינמיים לתוך מחרוזת. זה חוסך זמן, מונע טעויות והופך את הקוד שלנו לקריא ונוח יותר.

## כיצד לעשות את זה: 

 הנה איך אתה מכניס משתנים למחרוזת ב-PHP:

```PHP
$firstname = "אובאמה";
$snippet = "השם הפרטי שלי הוא $firstname.";

echo $snippet;
```

התוצאה שתופיע:

```
השם הפרטי שלי הוא אובאמה.
```

## הצצה לעומק:

היסטוריה: אינטרפולציה של מחרוזת מאפשרת מפרסרים עם מעט מאוד מאמץ להכניס נתונים למחרוזות. היא הייתה חלק מ-PHP 4. 

אלטרנטיבות: אנחנו יכולים גם להשתמש ב-concatenation במקום interpolation:

```PHP
$firstname = "אובאמה";
echo 'השם הפרטי שלי הוא ' . $firstname . '.';
```

איזון: עם זאת, כאשר אתה משלב מחרוזות, אתה צריך להוודא שאתה משתמש בגרשיים משלהם, כי לא תקבל את אותו התוצאה.

## ראה גם:

1. [המדריך המלא למחרוזות PHP](https://www.php.net/manual/en/language.types.string.php)
2. [אינטרפולציה של מחרוזות במדריך המרכזי](https://www.php.net/manual/en/language.types.string.php#language.types.string.parsing)