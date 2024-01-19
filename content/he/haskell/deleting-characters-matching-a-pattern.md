---
title:                "מחיקת תווים התואמים לתבנית"
html_title:           "Elixir: מחיקת תווים התואמים לתבנית"
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## מה זה ולמה?
מחיקה של תווים התואמים לתבנית היא פעולה המאפשרת לנו להסיר ממחרוזת תווים את כל התווים התואמים לתבנית מסוימת. מתכנתים עשויים לצלוח לביצוע הפעולה הזו כדי לטהר קלט, להפוך מידע לפורמט מסויים או לרפורמט מידע קיים.

## איך לעשות:
בעזרת הפונקציה `filter` שבספריית Prelude של השפה ניתן לבצע כאמור. לדוגמה:

```Haskell
נתון string = "להסיר תווים מסויימים ממחרוזת זו"
מנותר = filter (לא . (`elem` "אוי")) string
```
הפלט יהיה:

```Haskell
"להסר תווים מסוימים ממחרוזת זו"
```
בדוגמה זו הוסרו תווים מהמחרוזת ע"פ הצורך .

## צוללים עמוק יותר
- הקונטקסט ההיסטורי: Haskell הוא שפת תכנות פונקציונלית שפותחה בשנת 1990. עם זאת, ה"רעיון" של ביצוע פילטור על תווים במחרוזת הגיע לפני Haskell, על ידי שפות תכנות אחרות שכמוה עוסקות בעיבוד מחרוזות טקסט. 
- חלופות:קיימות שיטות אחרות להסרת תווים ממחרוזת, כולל שימוש ב-regEx או הפיכת המחרוזת לרשימה והסרת האיברים הרצויים. 
- פרטי היישום: בהשוואה לשפות תכנות אחרות, בהן יתכן להיות צורך בפיתוח הרבה יותר של קוד כדי להסיר תווים ממחרוזת, ב-Haskell ניתן לעשות זאת באופן מפושט להפליא באמצעות שימוש אינטואיטיבי בפונקציה 'filter'.
 
## ראו גם:
- [תיעוד 'filter'](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:filter): פרטים נוספים על הפונקציה 'filter' ב Haskell.
- [הדרך הפונקציונאלית](https://www.haskell.org/tutorial/): מלא מידע על פיתוח בהaskell.
- [עיבוד טקסט בHaskell](http://book.realworldhaskell.org/read/efficient-file-processing-regular-expressions-and-file-name-matching.html):מדריך לעיבוד מחרוזת.