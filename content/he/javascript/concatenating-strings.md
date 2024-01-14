---
title:    "Javascript: מחברת מחרוזות"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

##למה

התווספו יכולות חדשות לשפת ג'אווה סקריפט בתחום המחרוזת. מחרוזות מאפשרות לכתוב הודעות טקסט ולהביע דברים מורכבים בצורה יעילה יותר.

##כיצד להשתמש

כאשר צריך לצרוך מידע מסוגים שונים, ניתן לחבר אותם לביצוע פעולות מסוימות. לצורך כך, ניתן להשתמש במחרוזות כדי לאחד את המידע הנדרש. נהדר תיבל וניכל לשלב את המידע בקוד לתחילה ועם התוצאות הרצויות ישמש ל

"```Javascript
let str1 = "שלום"; 
let str2 = "לכולם"; 
let result = str1 + str2; 
console.log(result);
```"

Output: "שלום לכולם"

ניתן להשתמש גם בפונקציה מובנת בשפת ג'אווה סקריפט כגון "concat" לחיבור מחרוזות.

"```Javascript
let str1 = "מחרוזת";
let str2 = "concat";
let result = str1.concat(" ", str2);
console.log(result);
```"

Output: "מחרוזת concat"

##מעמקים

חיבור מחרוזות נחשבת לנושא מסובך יותר בשפת ג'אווה סקריפט, כך שמומלץ ללמוד בהימור קודם כי ישנם עקרונות קצת יותר מורכבים כמו עקרון התרגום. כמו כן, מומלץ לעבור על כמה פעמים על הקוד עם תרגילים שונים כדי להתאים לצורך לדימורדציה.

##!ראה גם

- פונקציה concat במפתח זוכת "MDN" (https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- פלט שפעולת החיפור במחרוזתים (https://www.w3schools.com/jsref/jsref_concat_string.asp)
- פעולת החיבור במחרוזות בשפה של ושל (https://www.youtube.com/watch?v=K-CrEi0ymMg)