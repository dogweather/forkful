---
title:                "חיפוש והחלפת טקסט"
html_title:           "Elm: חיפוש והחלפת טקסט"
simple_title:         "חיפוש והחלפת טקסט"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## מה ולמה?
חיפוש והחלפת טקסט הם פרוצדורות שמאפשרות למתכנת לאתר מחרוזת מסוימת בתוך טקסט ולהחליף אותה במחרוזת אחרת. התכנתים משתמשים בכך בעיקר לחסוך זמן ולייעל את התהליכים, כמו גם לתיקון באגים ושגיאות.

## איך:
הקוד הבא משתמש במתודה `replacingOccurrences` של Swift כדי לחפש טקסט מסוים ולהחליף אותו.
```swift
var str = "Hello, playground"
str = str.replacingOccurrences(of: "playground", with: "world")
print(str)
```
הפלט שנוצר מהקוד הוא:
```swift
Hello, world
```
בחלק זה ראינו כיצד משנים מחרוזת של טקסט באמצעות הפקודה `replacingOccurrences`.

## התנקזות:
במקרא דינמי כמו שלנו כיום, למתכנתים יש צורך תמידי להשתמש בכלים שיבחינו אותם מהזמן והמאמץ של כתיבת קוד מחדש. חיפוש והחלפה של סימנים התפתחו כתוך זמן והם אף הפכו לפונקציונליות בסיסית בכל שפת תכנות. Swift מספקת את המתודה `replacingOccurrences`, אך קיימות גם שיטות אחרות כמו `split` והרכבה חדשה של מחרוזות, או שימוש ב- Regular Expressions לביצוע תהליכים מורכבים יותר.

## ראו גם:
* [SwiftRocks](https://swiftrocks.com) - אתר המאגד מנות רבות נוספות על שפת Swift.
* [Stack Overflow](https://stackoverflow.com) - ניתן למצוא עזרה נוספת ודוגמאות לשפה Swift בקהילת Stack Overflow.