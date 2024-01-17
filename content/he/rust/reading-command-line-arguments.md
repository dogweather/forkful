---
title:                "קריאת ארגומנטים בשורת הפקודה"
html_title:           "Rust: קריאת ארגומנטים בשורת הפקודה"
simple_title:         "קריאת ארגומנטים בשורת הפקודה"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

מה ולמה?
קריאת ארגומנטים מפקודת השורה היא תהליך שבמהלכו מתכנתים קוראים ומנטרים את הפרמטרים שמשתמש מעביר לתוכנית בשורת הפקודה. זה מאפשר למשתמש לקבוע את התכונות וההתנהגות של התוכנית כאשר הוא מפעיל אותה. כמו כן, זכות המשתמש למסור פרמטרים מתאימים לתוכנית הופך את התוכנית ליותר נמדדת ומתאימה לצרכים שלו.

איך לעשות זאת:
קוד דוגמה:
```ראסט
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    
    println!("The program's name is: {}", args[0]);
    println!("The first argument is: {}", args[1]);
    println!("The second argument is: {}", args[2]);
}
```

פלט דוגמה:
```
$ ./program hello world
The program's name is: ./program
The first argument is: hello
The second argument is: world
```

עומק תחקיר:
בעבר, קריאת ארגומנטים מפקודת השורה הייתה תהליך מורכב יותר וכללה ביצוע של מספר שורות קוד כדי להשיג תוצאה דומה לזו שמופיעה כאן. ראסט משתמשת בספריית סטנדרטית המציעה פונקציות נוחות יותר כדי לקלקל את התהליך ולהפוך אותו לקצר ומתכוון יותר.

ראה גם:
למידע נוסף על קריאת ארגומנטים מפקודת השורה בראסט, ניתן לבדוק את המסמכים הרשמיים של ראסט ולצפות בדוגמאות נוספות. ניתן גם לחפש באינטרנט למדריכים ולפורומים שבהם משתמשים מציגים את יישום קריאת ארגומנטים מפקודת השורה במספר דרכים שונות.