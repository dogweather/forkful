---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "Go: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מה ולמה?
"המרת מחרוזת לאותיות קטנות" היא פעולה שבה אותיות גדולות במחרוזת משתנות לאותיות קטנות. מתכנתים מבצעים את הפעולה הזו כדי להפוך את התמחור לאחיד ולמנוע שגיאות רגישות לאותיות גדולות/קטנות.

## איך לעשות:
```Javascript
let myString = "Hello World!";
let lowerCaseString = myString.toLowerCase();
console.log(lowerCaseString); 
// יהיה "hello world!"
```

## העמקה:
להמרת מחרוזת לאותיות קטנות יש הקשרים ההיסטוריים שלה, שקשורים לשפות תכנות ישנות שהיו רגישות לאותיות גדולות/קטנות. בימים אלה, JavaScript מציעה למתכנתים שיטות אלטרנטיביות כמו `toUpperCase()` להמרת מחרוזת לאותיות גדולות. בנוגע לפרטי המימוש, JavaScript משתמשת בטבלת Unicode כדי למצוא את האות התואמת באותיות קטנות לכל אות במחרוזת.

## ראו גם:
- [מדריך JavaScript מפורט לעבודה עם מחרוזות](https://developer.mozilla.org/he/docs/Web/JavaScript/Guide/Regular_Expressions)
- [המרת מחרוזת לאותיות חסרוניות בשפת Python](https://stackoverflow.com/questions/6797984/how-do-i-lowercase-a-string-in-python)