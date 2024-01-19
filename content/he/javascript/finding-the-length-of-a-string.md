---
title:                "מציאת אורך המחרוזת"
html_title:           "Elm: מציאת אורך המחרוזת"
simple_title:         "מציאת אורך המחרוזת"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# מציאת אורך המחרוזת: מדריך קצר
## מה ולמה?
באופן מרכזי, מציאת אורך המחרוזת ב-Javascript (JS) כוללת ספירת התווים שבה. זה שימושי בהרבה תרחישים, למשל: כאשר אנחנו רוצים לוודא שהמשתמש הזין קלט מסוים.

## איך לעשות:
```Javascript 
var txt = "Hello World!";
var len = txt.length;
console.log(len);  // Prints: 12
```
בדוגמה הזו, אנחנו משתמשים במאפיין `.length` על המחרוזת `"Hello World!"` ומדפיסים את האורך שמצאנו.

## צלילה עמוקה
השפה Javascript היא אחת מהראשונות שהציגו מדד טבעי של אורך מחרוזות במאפיין `.length`. בעבר, בשפות אחרות, אנשי קוד היו צריכים לספור תווים באופן תוכניתי. 

ישנם אלטרנטיבות למאפיין `.length` ב-JS, אך אלה בדרך כלל משמשים למקרים נדירים. למשל: `Array.from(str).length` ימדד את האורך של מחרוזות עם תווים מיוחדים.

דע ביותר! `.length` מחזיר את מספר הייחידות הבסיסיות (code units) מסוג UTF-16 שבמחרוזת, לאו דווקא מספר התווים.

## כדאי לראות גם:
1. [MDN Web Docs: String.length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
2. [JavaScript Kit: String properties and methods](http://www.javascriptkit.com/jsref/string.shtml)
3. [StackOverflow: How does JavaScript .length property handle Unicode characters?](https://stackoverflow.com/questions/5438649/how-does-javascript-length-property-handle-unicode-characters)