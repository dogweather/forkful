---
title:                "עבודה עם מספרים מרוכבים"
date:                  2024-01-26T04:41:12.388263-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם מספרים מרוכבים"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?
מספרים מורכבים מורכבים מחלק ממשי וחלק מדומה (`a + bi`). הם שימושיים בתחומים שונים כמו הנדסת חשמל ומחשוב קוונטי. מתכנתים משתמשים בהם כדי למדל משוואות שאין להן פתרון בעזרת מספרים ממשיים בלבד.

## איך לעשות:
ל-Gleam אין תמיכה ילידית במספרים מורכבים. בדרך כלל תגלגל לעצמך או תמצא ספרייה. הנה דוגמה מהירה לכיצד ניתן לממש פעולות בסיסיות:

```gleam
type Complex {
  Complex(Float, Float)
}

fn add(c1: Complex, c2: Complex) -> Complex {
  let Complex(a, b) = c1
  let Complex(x, y) = c2
  Complex(a + x, b + y)
}

fn multiply(c1: Complex, c2: Complex) -> Complex {
  let Complex(a, b) = c1
  let Complex(x, y) = c2
  Complex(a*x - b*y, a*y + b*x)
}

fn main() {
  let num1 = Complex(1.0, 2.0)
  let num2 = Complex(3.0, 4.0)
  let sum = add(num1, num2)
  let product = multiply(num1, num2)

  sum // Complex(4.0, 6.0)
  product // Complex(-5.0, 10.0)
}
```

## צלילה עמוקה

מספרים מורכבים נתונו תיעוד רשמי לראשונה על ידי ג'רולמו קרדאנו במאה ה-16. הם הרחבה טבעית של המספרים הממשיים. עם זאת, בשפה צעירה כמו Gleam - שמעדיפה ביצועים ובטיחות טיפוס - תכונות כאלה הן בסיסיות (או שאתה עושה זאת בעצמך).

בחלק מהשפות האחרות, כמו Python, מספרים מורכבים הם מובנים (`3+4j`), מה שהופך את החיים לקלים יותר. ב-Rust או ב-Haskell יש ספריות שמציעות פונקציונליות מתקדמת מיד.

הגישה של Gleam אומרת שעליך להתמודד עם כל ההיבטים: אריתמטיקה, קואורדינטות פולריות, צורות אקספוננציאליות וכדומה. לממש פעולות יעילות ומדויקות דורשת תכנות זהיר, בהתחשב באופן שבו התנהגות נקודה צפה יכולה להשפיע על התוצאות שלך.

זכור לבדוק היטב, בפרט מקרי קצה! התמודדות עם אינסוף מורכב וערכי NaN (מספר לא מוגדר) יכולה להטעות אותך אם אתה לא זהיר.

## ראה גם
לעוד דברים טובים, הנה היכן אתה יכול לצלול:
- [התיעוד הרשמי של Gleam](https://gleam.run/documentation/)
- חפש השראה בספריות של שפות אחרות, כמו של Rust [num-complex](https://crates.io/crates/num-complex) או המודול [cmath](https://docs.python.org/3/library/cmath.html) של Python.
