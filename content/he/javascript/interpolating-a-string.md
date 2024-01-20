---
title:                "אינטרפולציה של מחרוזת"
html_title:           "Arduino: אינטרפולציה של מחרוזת"
simple_title:         "אינטרפולציה של מחרוזת"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/interpolating-a-string.md"
---

{{< edit_this_page >}}

# מה זה ולמה? 

אינטרפולציה של מחרוזות היא הטמעת ביטויים או משתנים בתוך מחרוזת מהודרת. מתכנתים משתמשים בטכניקה זו להצגת משתנים בצורה נוחה וקלה לקריאה במחרוזות.

# איך לעשות זאת:

ב-Javascript, אנחנו משתמשים במחרוזות מהודרות (template strings) לאינטרפולציה של מחרוזות. ניתן ליצור מחרוזת מהודרת על ידי שימוש בסוגריים מרובעים (` `) במקום מרכאות רגילות (' 

לדוגמא:

```Javascript
let name = "דני";
let message = `שלום, ${name}!`;
console.log(message); // שלום, דני!
```

# צלילה עמוקה:

1. הקונטקסט ההיסטורי: בעבר, מתכנתים של Javascript היו נאלצים להשתמש בשיטות מורכבות כמו חיבור של מחרוזות באמצעות + או שימוש במתודות כמו `string.concat()` כדי ליצור מחרוזות משולבות. אבל מאז ES6, האינטרפולציה של מחרוזות הופכת להבנה ולשימוש קלים.

2. אלטרנטיבות: כיצד לאמצעים אחרים כדי לבצע את אותה הפונקציונליות? מחרוזות ממולאות יכולות לבצע את אותה המשימה תחליף. לדוגמא:

```Javascript
let name = "דני";
let message = 'שלום, ' + name + '!';
console.log(message); // שלום, דני!
```

3. פרטי הרצה: JavaScript מחליף ביטויים במחרוזת מהודרת בערכם של הביטוי, ממירה את התוצאה למחרוזת אם זו לא מחרוזת, ואז מתחברת לשאר המחרוזת.

# ראה גם: 

1. [Template literals (Template strings) - JavaScript | MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)