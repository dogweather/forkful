---
title:    "Swift: קריאת פרמטרים משורת הפקודה"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## למה

קריאת ארגומנטי שורת הפקודה יכולה להיות חיונית כאשר מתכנתים משתמשים בשפת סוויפט. הם מאפשרים לך לפענח פרמטרים שהועברו כחלק מהפקודה הנתונה ולבצע פעולות מתאימות עליהם. קריאת ארגומנטי שורת הפקודה מאפשרת לך לתכנת אפליקציות גמישות יותר שמתאימות לצורך של המשתמש.

## כיצד לעשות זאת

אתם יכולים לקרוא ארגומנטי שורת הפקודה בסוויפט באמצעות המחלקה `CommandLine`. הנה דוגמה פשוטה:

```Swift
let arguments = CommandLine.arguments

if arguments.count > 1 {
  print("הארגומנט הראשון הוא: \(arguments[1])")
} else {
  print("לא ניתן למצוא ארגומנטים")
}
```

פלט יהיה הטקסט המופיע כאשר אתם מפעילים את האפליקציה עם ארגומנטים:

```
> MyApp Hello
הארגומנט הראשון הוא: Hello
```

אם אתה מפעיל את האפליקציה ללא ארגומנטים:

```
> MyApp
לא ניתן למצוא ארגומנטים
```

## חקר מעמק קריאת ארגומנטי שורת הפקודה

כדי לקבל ידע מעמיק יותר על קריאת ארגומנטי שורת הפקודה בסוויפט, אתם יכולים לחפש נושאים נוספים כגון: טיפול בארגומנטים מסוימים, שימוש במאפיינים כמו `CommandLine.Option` ועוד.

## ראה גם

למידע נוסף על קריאת ארגומנטי שורת הפקודה בסוויפט, תוכלו לבדוק את המקורות בקישורים הבאים:

- [מדריך רשמי של אפל](https://developer.apple.com/documentation/swift/commandline).
- [פוסט בבלוג על קריאת ארגומנטי שורת הפקודה בסוויפט](https://