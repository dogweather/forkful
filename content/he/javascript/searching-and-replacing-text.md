---
title:    "Javascript: חיפוש והחלפת טקסטים"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## למה
בעולם התכנות, תחליפי חיפוש והחלפה של טקסט הם כלי חיוניים למניעת החזרת אותם פעמים רבות ולהפיכת הקוד לנקי ומנוסח.

## איך לעשות זאת
הנה כמה דוגמאות של כיצד לתמוך בחיפוש והחלפה במקטעי קוד:

```Javascript
// חיפוש מספר במערך והחלפתו
let numbers = ["אחד", "שני", "שלוש", "ארבע", "חמישה"];
numbers.map(function(num) {
    // בודק אם המספר מתחיל עם האות "א"
    if (num.startsWith("א")) {
        // מחליף את המספר ב- "א"
        num = "א";
    }
    return num;
}); 
// פלט: ["אחד", "שני", "שלוש", "א", "חמישה"]

// חיפוש והחלפה של כל המופעים של מחרוזת מסוימת
let sentence = "שלום, שלום, שלום";
let newSentence = sentence.replace(/שלום/g, "היי");
// פלט: "היי, היי, היי"
```

## העמקת מבנה
החיפוש והחלפה של טקסט יכול להתבצע באמצעות מתודות פנימיות כמו `replace()` או באמצעות תבניות רגולריות כמו בדוגמה השנייה. בנוסף, ישנם פתרונות חיצוניים שניתן להשתמש בהם כדי להקל על המשימה כמו תוכניות של עורכי טקסט או תוכניות של עיבוד תמונה.

## ראו גם
- [מדריך לחיפוש והחלפת טקסט ב-Javascript](https://www.keycdn.com/blog/javascript-string-replace)
- [אריזת Regex הישראלית לפי mama.js](https://sourceforge.net/projects/regextester/)
- [תוכניות חיפוש והחלפה ב-Chrome Web Store](https://chrome.google.com/webstore/search/search%20and%20replace?hl=he)