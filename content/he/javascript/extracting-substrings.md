---
title:                "חילוץ תת-מחרוזות"
html_title:           "Bash: חילוץ תת-מחרוזות"
simple_title:         "חילוץ תת-מחרוזות"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## מה זה ולמה? 
משיכת הביטוי החלקי (Substring) הוא התהליך שבו אנו מאחזרים חלק ממחרוזת. מתכנתים עשויים לבצע את זה כאשר הם רוצים להבין, לנתח או לשנות במשתמשים מידע מסוים בתוך מחרוזת גדולה יותר.

## איך לעשות:
ב-JavaScript, אנו משתמשים במתודות substring(), substr(), או slice() לחלצת ביטויים חלקיים. הנה דוגמאות:

```Javascript
var str = 'Hello, World!';
console.log(str.substring(0, 5)); // פלט: Hello
console.log(str.substr(7, 5)); // פלט: World
console.log(str.slice(-1)); // פלט: !
```

## צלילה עמוקה:
הצורך לחלץ מחרוזות חלקיות התחיל מתחילת התכנות. בעבר, היינו משתמשים באופרטורים על מחרוזות, אך זה היה מסורבל ותלוי בשפה. JavaScript הקלה עלינו את החיים עם מתודות פשוטות.

slice(), substring(), ו- substr() יכולות להיות מבלבלות, אך ההבדל הוא באופן שבו הן מנהלות את האינדקסים. slice() ו- substring() רואות את המספר השלישי כסוף המחרוזת, בעוד ש- substr() רואה את המספר השני כאורך המחרוזת.

אם אתה מעוניין לפרטים נוספים, בדוק את התיעוד הרשמי של MDN.

## עיין גם:
1. [MDN - String.prototype.substring()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
2. [MDN - String.prototype.substr()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substr)
3. [MDN - String.prototype.slice()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
4. [W3Schools - JavaScript String Methods](https://www.w3schools.com/js/js_string_methods.asp)