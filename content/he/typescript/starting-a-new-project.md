---
title:                "התחלת פרויקט חדש"
html_title:           "TypeScript: התחלת פרויקט חדש"
simple_title:         "התחלת פרויקט חדש"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## למה

אז למה מישהו יחליט להתחיל פרויקט חדש ב TypeScript כשכבר יש כל כך הרבה שפות תכנות אחרות לבחירה? הסיבה הבאה פשוטה מאוד: TypeScript מספק יכולת חזקה יותר של תכנות מונחה עצמים מאשר ספריפט של JavaScript בלי לכאורה להפסיד את הגמישות והאפשרויות הרבות של JavaScript. אם אתה מתחיל פרויקט גדול ורוצה להיות בטוח שהקוד שלך עובד בצורה נכונה ואינו מכיל באגים, TypeScript הוא בחירה מעולה עבורך.

## איך לעשות זאת

לפניכם כמה דוגמאות לקוד TypeScript שתראו כיצד לכתוב בצורה נכונה במספר תחומים שונים. תהנו!

```TypeScript
// משתנה מספרי
let num: number = 5;

// מחרוזת
let str: string = "Hello World";

// פונקציה עם פרמטרים
function addNums(x: number, y: number): number {
  return x + y;
}

// קלאס עם פרופרטי ומתודה
class Car {
  model: string;
  year: number;
  
  constructor(model: string, year: number) {
    this.model = model;
    this.year = year;
  }
  
  accelerate() {
    console.log("Vroom Vroom!");
  }
}

// יצירת אובייקט וקריאה למתודה
let myCar = new Car("Tesla", 2021);
myCar.accelerate();
```

## Deep Dive

כדי להתחיל פרויקט חדש ב TypeScript, כדאי לקחת בחשבון את הנקודות הבאות:

- לעשות בדיקה אם הסביבה שלך מתאימה להרצה של TypeScript. ניתן להוריד את הסביבה הדרושה מהאתר הרשמי של TypeScript.
- ללמוד על הנהלים והכלים הסטנדרטיים של TypeScript כדי להיות מוכנים לכתוב קוד בצורה נכונה ולמנוע באגים מיותרים.
- כדאי להשתמש בכלים מתאימים (כגון Visual Studio Code) שיסייעו לך בכתיבת הקוד באופן יעיל יותר ויאפשרו לך להתרכז רק בכתיבת הקוד