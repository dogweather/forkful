---
title:                "הורדת דף אינטרנט"
html_title:           "C++: הורדת דף אינטרנט"
simple_title:         "הורדת דף אינטרנט"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## מה ולמה?

הורדת דף אינטרנט, זו הפעולה של שליפת קוד HTML של דף אינטרנט ושמירתו על המחשב של ך. מתכנתים עשויים לבצע זאת כדי לנתח, לבדוק או לשמור על נתונים מסוימים לשימוש במועד מאוחר יותר.

## איך:

מדריך קצר על כיצד להוריד דף אינטרנט באמצעות Node.js וביבליות 'axios' ו'cheerio'.

```TypeScript
import axios from 'axios';
import cheerio from 'cheerio';

async function downloadHTML(url: string):Promise<void>{
  const response = await axios.get(url);
  const $ = cheerio.load(response.data);
  const content = $('body').html(); 
  console.log(content); 
}

downloadHTML('http://google.com');
```
בכול פעם שאתה מריץ את הקוד הזה, הוא יוריד את קוד ה־HTML של גוף הדף של Google וידפיס אותו במסוף.

## הצצה עמוקה

הורדת דף אינטרנט כרגע הייתה חלק מתכנות הפקה מאז ימי האינטרנט הראשונים. למרות שלמרבה המקרים בניית API העשירה המאפשרת גישה ישירה לנתונים הדרושים היא הפתרון המועדף, לא תמיד מידע זה זמין או נגיש.

חלפה מגוון של טכנולוגיות כמו `XMLHttpRequest` ו`fetch` בצד לקוח ב־JavaScript או אפילו ביבליות `cURL` בצד שרת ב־PHP, אך בסביבות Node.js החשובות 'axios' ו'cheerio'  נהיו נפוצות בזכות הפשטות שלהם והכוח שהם מאפשרים.

## ראה גם

- [עוד על 'axios'](https://axios-http.com/docs/intro)
- [עוד על 'cheerio'](https://cheerio.js.org/)
- [הדרך המודרנית לאתחל את Node: TypeScript, VS Code, Typings](https://medium.com/javascript-scene/the-modern-way-to-run-jest-tests-in-typescript-36e07ee6fb3d)
- [הכל על TypeScript (המדריך המלא)](https://www.typescriptlang.org/docs/handbook/typescript-in-5-minutes.html)