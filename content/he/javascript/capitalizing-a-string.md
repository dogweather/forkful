---
title:    "Javascript: כתיבת ראשי בתוכנות מחשב"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## למה

מתי נדרש לכתוב קוד שיכול להפוך את מחרוזת לתווית ראשות?

## כיצד לעשות זאת

הנה דוגמאות קוד ופלט כדי ללמוד איך לכתוב קוד בפעולת רישום תווית במחרוזת. ניתן להשתמש בקוד המופיע עם [לולאה "for"](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/for), [תורת החלוקה `.split()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/split) ומאחרי זה לתחום את ראש הרישום תווית עם [תורת ההפיכה `.toUpperCase()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase).

 ```Javascript
// דוגמאות קוד כדי להפוך מחרוזת לרשות ראשון בעזרת לולאה for והפיכת תווית לאותיות ראשון גדולות
let stringToCapitalize = "מחרוזת להפוך לאותיות ראשונות גדולות";
let stringArray = stringToCapitalize.split(" ");
let capitalizedString = "";

for (let i = 0; i < stringArray.length; i++) {
  let capitalizedWord = stringArray[i][0].toUpperCase() + stringArray[i].slice(1);
  capitalizedString += capitalizedWord + " ";
}

console.log(capitalizedString); //פלט: "מחרוזת להפוך לאותיות ראשונות גדולות"
```

## עקרונות ראשונים עמוקים

רישום תווית הוא הפעולה של עבודת התרגום ממחרוזת, או מסדרת תווים, למחרוזת אחרת עם ראשון תווית באותיות ראשונות גדולות. הפעולה זו נדרשת בעיקר כאשר אנחנו עובדים עם תוכנה המוצגת מידע כמו בתוך אבאלוני הבלוגים, בעקבות מונגווזי DB ועוד. דוגמאות להמחרוזת כוללת:

- כותרת של כתבות להצגה
- משובים מלקוחות במאגרי מידע
- ותוכן אחר

## ראה גם

- [תורת ההפיכה `toUpperCase()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [תורת החלוקה `.split()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/split)
- [לולאה "for"](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/for)