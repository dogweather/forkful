---
title:                "מחיקת תווים התואמים לתבנית"
html_title:           "Elixir: מחיקת תווים התואמים לתבנית"
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# מחיקת תווים התואמים לדפוס ב- TypeScript: הדרך הקלה

## מה ולמה?

כאשר אנחנו מתייחסים ל"מחיקת תווים התואמים לדפוס", אנחנו מתייחסים להסרת תווים מחרוזת שמתאימה לדפוס מסוים, באמצעות ביטויים רגולאריים או מחרוזות תת. מתכנתים עושים זאת כדי לנקות ולתקן נתונים, כמו מחיקת רווחים מיותרים מסביב למחרוזת.

## איך לעשות את זה:

נוכל להשתמש בשיטת replace של מחרוזת ובביטוי רגולארי:

```TypeScript
let myString = 'שלום עולם!  עשרה   ';
myString = myString.replace(/\s+/g, ' ').trim();
console.log(myString); // "שלום עולם! עשרה"
```

בקוד החלופי, אנחנו מציאים את כל הרווחים המיותרים ומחליפים אותם ברווח יחיד. השימוש ב- trim נותן לנו להסיר את הרווחים משני הקצוות של המחרוזת.

## צלילה עמוקה:

השמת דפוסים מתווים מאוד מוכרת בתכנות. היא נוסעת אחורה לימי שפת התכנות Perl, שאחראית לביטויים הרגולאריים המודרניים שלנו. ב- TypeScript, אנחנו יכולים להשתמש בפעולת replace כחלופה.

חשוב לציין שישנם שיטות אחרות למחיקת דפוסים מתווים ב-TypeScript ושפות תכנות אחרות, כמו split ו- join, אך בדרך כלל הם מתבצעים באופן כמעט זהה, וההבדלים נובעים בעיקר מממשק ה- API.

דינמיקות ביטוי הרגולארי המשמשת כאן, '/\s+/g', מייצגת "איתור תווים של רווחים אחד או יותר".

## ראה גם:

- [מדריך לביטויים רגולאריים ב- TypeScript](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-0.html)
- [הסרת תווים מתחילת או מהסוף של מחרוזת](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/Trim)
- [שימוש במתודה replace ב-TypeScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)