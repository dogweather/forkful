---
title:                "מציאת אורך עבור מחרוזת"
html_title:           "TypeScript: מציאת אורך עבור מחרוזת"
simple_title:         "מציאת אורך עבור מחרוזת"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# מדועלמה אדם יתעסק במציאת אורך של מחרוזת?

המצאת אורך של מחרוזת היא חלק חשוב בתהליך פיתוח תוכנה, שכן לעיתים קרובות תידרש לנו להשתמש באורך של מחרוזת כדי לטפל בה כראוי, לתנאים ספציפיים, או לצורך טיפול בשיבושים פוטנציאליים. דרך אחת לעשות זאת היא לשים לב למציאת אורך המחרוזת המתאים לצורך מסוים.

## איך למצוא את אורך המחרוזת באמצעות "TypeScript"

בדוגמאות הקוד הבאות תוכלו לראות איך למצוא את אורך המחרוזת באמצעות TypeScript. נתחיל עם פונקציית "length" המשמשת לקבלת אורך המחרוזת:

```TypeScript
// יצירת משתנה של מחרוזת המכילה שמות של פירות
let fruits: string = "תפוח, בננה, אבטיח";

// הדפסת אורך המחרוזת באמצעות פונקציית "length"
console.log(fruits.length); // הודפס: 16
```

בדוגמה הבאה, נמצא את אורך המחרוזת באמצעות "for loop":

```TypeScript
// יצירת משתנה של מחרוזת המכילה שם של עיר
let city: string = "תל אביב";

// יצירת משתנה לתחילת מונה
let count: number = 0;

// לולאת "for" שמספרת את התווים במחרוזת ומעדכנת את משתנה המונה
for (let i = 0; i < city.length; i++) {
  count++;
}

// הדפסת אורך המחרוזת
console.log(count); // הודפס: 7
```
לבסוף, בסקריפט שלפניכם תוכלו למצוא איך להשתמש בפונקציה של "Array.from" למציאת אורך המחרוזת:

```TypeScript
// יצירת משתנה של מחרוזת המכילה את הקוד הפועל ב HTML
let code: string = "<html><body><h1>Hello World!</h1></body></html>"

// יצירת מערך חדש מהמחרוזת עם השימוש בפונקצי