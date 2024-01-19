---
title:                "קריאה של ארגומנטים משורת הפקודה"
html_title:           "C#: קריאה של ארגומנטים משורת הפקודה"
simple_title:         "קריאה של ארגומנטים משורת הפקודה"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## מה ולמה?
קריאה של ארגומנטים משורת הפקודה היא התהליך של שליפת מידע מקלט המוזן על ידי המשתמש בגרמנט של הישום. מתכנתים מרבים להשתמש בכך כדי להקל על עיבוד ובקרה במהלך שליטה של יישום.

## איך ל:
אנחנו יכולים לראות את הארגומנטים של Swift לשורת הפקודה באמצעות הפרופרטי CommandLine.arguments:

```Swift
for argument in CommandLine.arguments {
    print(argument)
}
```
כאשר מריצים את הקוד מעלה, נקבל את התוצאות בהתאם לארגומנטים שמוזנים משורת הפקודה.

## השקיעה לתוך:
הארגומנטים לשורת הפקודה הם דרך קלסית כבר מימי שפות התכנות הראשונות. חלופות להם כוללות קבצי תצורה ומשתנים סביבתיים. המקרה הספציפי של Swift משוייך עם רענון בסדר הארגומנטים מפעם לפעם ותמיכה בניתוח ארגומנטים המוזנים משורת הפקודה.

## ראו עוד:
הקישורים הבאים מציעים מידע נוסף:
- [Apple Swift Documentation - Command Line Arguments](https://developer.apple.com/documentation/swift/commandline)
- [Stack Overflow - How to access command line arguments](https://stackoverflow.com/questions/24001943/how-to-access-command-line-arguments-in-swift)