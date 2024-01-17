---
title:                "הורדת דף אינטרנט"
html_title:           "Arduino: הורדת דף אינטרנט"
simple_title:         "הורדת דף אינטרנט"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

כתיבת תכנות עם ארדואינו - למה ואיך?

## מה זה ולמה? 

 להוריד עמוד אינטרנט זה להוריד מידע מהרשת העולמית למחשב או למכשיר אחר. תוכניות תכנותיות משתמשות ביכולת זו בכדי לאחזר מידע חיוני כמו את נתוני המזג או את מחירי המניות.

## איך לעשות:

הנה שני דוגמאות של קוד ארדואינו להורדת עמוד אינטרנט ותוצאת הפלט: 

```
Arduino.begin(); 
webpage = download("https://www.example.com"); 
print(webpage);
```

```
Arduino.begin(); 
webpage = download("https://www.example.com"); 
print(text(webpage));
```

## המעמד לעומק:

לפני תיכנות מחשבים, היו חלופות להורדת דפים אינטרנט כמו המדפסת טארטלוט והדפדפן. מתוך  החלפת התוכן והדמיון, תכניות תוכנות יכולות לגורל מיוחד בין מידע ממקור שונה.

## ראו גם:

כדי ללמוד עוד על תכניות תכנות עם ארדואינו: https://www.arduino.cc/en/Guide/HomePing 

 עמוד רשמי של תכנת ארדואינו: https://www.arduino.cc/