---
title:                "Javascript: האכסנת html"
simple_title:         "האכסנת html"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## למה

להפעיל למראה ("parsing") של קוד HTML נחוץ כאשר נשים בכתיבת קוד גלישת אינטרנט מתקדמת ("web scraping"), בונים אפליקציות דינאמיות או מבצעים ניתוח נתונים.

## איך לעשות זאת

בשפת ג'אווהסקריפט ("Javascript"), ישנן מספר סיפורות ("libraries") שיכולות לעזור בהפעל למראה של קוד HTML. לדוגמה, הסיפורים "Cheerio" ו-"jsdom" מאפשרים לנו לבצע ריטריב פריטים מתוך קוד HTML ולשנות אותם לאובייקטים בשפת ג'אווהסקריפט.

בהמשך נמצא קוד נייד עם הסברים ופלט דוגמאות:

```Javascript
const request = require('request');
const cheerio = require('cheerio');

// הפעל את שיטת GET באמצעות הספרייה "request"
request('https://en.wikipedia.org/wiki/Paris', function(error, response, html){
    if(!error && response.statusCode == '200'){
        // אחרי שקיבלנו את העמוד של פריז, נבצע ניפוי של המידע שחיפשנו באמצעות "cheerio"
        const $ = cheerio.load(html);
        
        // שפרסמנו את שם העיר של פריז מתוך עמוד הויקיפדיה שלה
        console.log($('#firstHeading').text());
    }
});
```

פלט:

```
Paris
```

## Deep Dive

בעזרת הספרייה "jsdom", אנו יכולים להפעיל למראה של קוד HTML כדי לא רק ליצור את המודל של הדף אבל גם לבצע שינויים בדף עצמו. ניתן להראות דוגמה פשוטה ליצירת אלמנטים באמצעות קוד מוצלח אחר:

```Javascript
const jsdom = require("jsdom");
const { JSDOM } = jsdom;

// הפעל את המתודה של "JSDOM" עם הבאנדל של המעברים
const dom = new JSDOM(`<!DOCTYPE html><p>Hello world</p>`, {
    // אם אתה רוצה לאמן רשימה של תכונות עבור אלמנטים, יש אפשרות לשינוי הרשימה הזו
    // אחרי שהדף סיים להיטען, בשבילי יש הרבה סטיילים בשביל הפונ