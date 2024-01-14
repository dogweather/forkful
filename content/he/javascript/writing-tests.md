---
title:                "Javascript: כתיבת בדיקות"
programming_language: "Javascript"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/writing-tests.md"
---

{{< edit_this_page >}}

למה:

כתיבת בדיקות יכולה להיות כלי עזר חשוב בתהליך הפיתוח. הבדיקות מאפשרות לנו לבדוק את הקוד שלנו ולוודא שהוא עובד באופן תקין ולכן, הם עשויים לסייע לנו למצוא באגים ושגיאות ביעילות גבוהה יותר.

כיצד להשתמש:

כדי לכתוב בדיקות יעילות בג'אווהסקריפט, ניתן להשתמש בפונקציות כמו describe ו-it כדי לארגן את הקוד שלנו. הנה דוגמא של כתיבת בדיקת יחידה לפונקציה שמחשבת את הממוצע של מערך:

```Javascript
describe("calculateAverage", ()=> {
  it("should return the correct average", ()=> {
    const arr = [2, 3, 4, 5];
    const result = calculateAverage(arr);
    expect(result).toBe(3.5);
  });
});
```

תהוריה עמוקה:

הבדיקות הם חלק אינטגרלי של תהליך הפיתוח. הן מאפשרות לנו להיות בטוחים שהקוד שלנו עובד כצפוי וכך מסייעות לנו להפחית את המעיכה בעת הכתיבה של קוד חדש או שינויים ולשמור על איכות הקוד. בנוסף, הבדיקות עוזרות למתכנתים להבין באופן יעיל יותר את הקוד שלהם ולגלות באגים ושגיאות בצורה מהירה יותר.

ראה גם:

- בלוג (באנגלית): "כיצד לכתוב בדיקות נוחות ויעילות בג'אווהסקריפט" מאת ג'ק הוריגן (https://medium.com/@jackal_h/on-javascript-testing-f8982b2e24c6)
- ספר (באנגלית): "בדיקת וין: מנועי נסיעה בג'אווהסקריפט ומתודולוגיות הבדיקה שלהם" מאת סטיבן ראדייג (https://www.amazon.com/Testing-Vue-js-Applications-Steven-Roberts/dp/1680505544)
- מדריך (בעברית): "מנוע הבדיקות Mocha והמרחבי מבטא חריץ Chrome" מאת יונתן/אל