---
title:                "מציאת אורך מחרוזת"
html_title:           "Javascript: מציאת אורך מחרוזת"
simple_title:         "מציאת אורך מחרוזת"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## מה זה ולמה?
מציאת אורך של מחרוזת היא תכונת פונקציונלית בשפת ג'אווהסקריפט שמאפשרת למתכנתים לקבל את מספר התווים במחרוזת. זה מאפשר מתכנתים לנהל ולעבד מחרוזות יעיל יותר, במיוחד כאשר משתמשים במתודות של שפת ג'אווהסקריפט המתבססות על מספר התווים במחרוזת.

## איך לעשות זאת:
הנה כמה דוגמאות לכתיבת הקוד למציאת אורך של מחרוזת ותוצאת מספר התווים בה: 
```Javascript
// דוגמא 1:
let str = "זו מחרוזת לדוגמה";
console.log(str.length);

// תוצאה צפויה: 16

// דוגמא 2:
let input = prompt("נא להזין מחרוזת:");
console.log(`המחרוזת שהזנת יש בה ${input.length} תווים`);

// תוצאה משתנה בהתאם למחרוזת שהמשתמש מזין
```

## עומק כיוון
- **היסטורי:** תכונת מציאת אורך של מחרוזת נוצרה כחלק מתכניות שפת ג'אווהסקריפט בשנות ה-90 כדי לתמוך בעיבוד טקסט באמצעות תכונות ופונקציונליות מתקדמות יותר.
- **אלטרנטיבות:** ישנן פתרונות אלטרנטיביים למציאת אורך של מחרוזת בשפת ג'אווהסקריפט, כגון שימוש במתודת **slice** או **substring**.
- **פרטי יישום:** תכונת מציאת אורך של מחרוזת מיושמת באמצעות אלגוריתם שמספר כמה תווים נמצאים בתוך המחרוזת ומחזירה את התוצאה בצורה נוחה לשימוש.

## ראה גם:
- [תיעוד שפת ג'אווהסקריפט על תכונת מציאת אורך של מחרוזת](https://developer.mozilla.org/he/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [מדריך למתחילים בתכונת מציאת אורך של מחרוזת בשפת ג'אווהסקריפט](https://www.w3schools.com/jsref/jsref_length_string.asp)
- [אפשרויות אלטרנטיביות למציאת אורך של מחרוזת בשפת ג'אווהסקריפט](https://www.codecademy.com/courses/introduction-to-javascript/lessons/string-length/exercises/introduction)