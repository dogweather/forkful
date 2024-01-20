---
title:                "ניתוח HTML"
html_title:           "Arduino: ניתוח HTML"
simple_title:         "ניתוח HTML"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/parsing-html.md"
---

{{< edit_this_page >}}

# מה זה ולמה? 
פרסונג של HTML הוא התהליך שבו נתונים מפורמטים מהופכים לאוביקטים תוכניתיים שניתן להשתמש בהם. מתכנתים עושים את זה כדי לגעת באלמנטים DOM מתוך משאית קוד HTML.

# איך לעשות:
ניתן לבצע פרסונג של HTML באמצעות 'DOMParser'. הראה דוגמא לביצוע זה:
```Javascript
var parser = new DOMParser();
var htmlDoc = parser.parseFromString('<p>Hello World</p>', 'text/html');

console.log(htmlDoc.body.textContent); // "Hello World"
```
הפונקציה parseFromString מאפשרת יצירת אוביקט DOM שהוא מראה של ה-HTML המקורי. 

# צלילה עמוקה: 
אף פעם לא מיותר לדעת בחדשות. DOMParser הוא חלק מתקן ה-W3C המשערת שהפקודה היא חלק תוך כדי בניית התקן Document Object Model. קיימות גם אופציות חלופיות, כמו jQuery's parseHTML, אך הן מאפשרות רק חלק מהיכולות של DOMParser. כמו כן, על אמצעים כמו DOMParser לעבוד באופן שקף ברוב הדפדפנים, מה שמקל על העבודה היומיומית שלנו כמתכנתים.

#ראה גם
- [מסמך MDN על DOMParser](https://developer.mozilla.org/en-US/docs/Web/API/DOMParser)
- [הבנת שפת ה-HTML](https://he.khanacademy.org/computing/computer-programming/html-css)