---
title:                "Arduino: חילוץ תגיות אתרים"
simple_title:         "חילוץ תגיות אתרים"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## למה

מתכנתי ארדואינו לעתים קרובות נתקלים בצורך לעבוד עם קוד HTML, וכדי לתמוך בפעולות כמו שאילתא או הצגת מידע במסך OLED, נדרשת היכולת לפרק את הקוד HTML לחלקיו השונים. לכן, יש מספר סיבות לפרק קוד HTML מתוך תוכניות ארדואינו.

## איך לבצע פעולת פרקטט HTML בארדואינו

כדי לפרק קוד HTML בארדואינו, ניתן להשתמש במספר ספריות ופקודות הזמינות עבור משתמשי ארדואינו:
```c++
// הכנס את הטקסט המלא של הקוד HTML
String htmlCode = "<html><head><title>כותרת</title></head><body><h1>שלום עולם!</h1></body></html>";

// יצירת משתנה שבו נאחסן את התוכן של התג <title>
String title;

// חיתוך הקוד HTML לאורך התווים "<title>" ו "</title>"
int titleStartIndex = htmlCode.indexOf("<title>") + 7; // + 7 מטורף את כמות התווים בתג כדי להמציא את אינדקס התווים הראשונים של התג
int titleEndIndex = htmlCode.indexOf("</title>");

// חיתוך הטקסט שבין התווים כדי לקבל את התוכן של התג <title>
title = htmlCode.substring(titleStartIndex, titleEndIndex);

// הצגת התוכן על המדפסת הסדוקה
Serial.println(title); // יוצא "כותרת"
```

## לחטוף לילו יונים נעל כ מקור

כאשר מבצעים את פעולת פרק הקוד HTML בארדואינו, חשוב לזכור האת התגים מכילים רק מידע נוסף מה חלקי אדם. כמו כרום Javascript, קוד HTML מתכלה למספר מילים שונות, וידוע שהתוכן משתנה בכתיב בקושי. לכן, כדאי לבחור ספריות המסייעות בפעולת פרק הקוד HTML, כמו למשל ספריית [this HTML Parser](https://playground.arduino.cc/Main/htmlparse/) או [this ESP8266 HTML Parser](https://github.com/this/ESP8266_HTML