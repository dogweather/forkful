---
title:                "Arduino: חיפוש והחלפה של טקסט"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## למה:

למה לחפש ולהחליף טקסט? דבר ראשון, זה יכול לחסוך המון זמן ועבודה בניסיונות לכתוב קוד. בנוסף, זה יכול לסייע לנו לנהל עלות נמוכה יותר על יישומים מכאניים ולגזור על חומרים נדרשים לעבודה.

## איך לעשות זאת:

כאשר מדובר בטקסט, ישנן שתי אפשרויות לחיפוש והחלפתו: “חיפוש” ו”החלקה”.

באמצעות פונקציות חיפוש תוכלו לחפש מחרוזת קבועה בתוך מחרוזת אחרת ולאתר את מיקומה, תמיד אל תשכחו להתאים את הפורמט במחרוזת שלכם למחרוזת המחפשת באמצעות תווים או regex.

בבאמצעות פונקציות החלפה, תוכלו למצוא ולהחליף חלקים של מחרוזת בחלקים אחרים. למשל, ניתן להחליף כמה אותיות במילה או להחליף מחרוזות שלמות.

נהדר! עכשיו שאתם מבינים את המושג, בואו נראה כמה דוגמאות קוד כדי להמחיש את השימוש של פונקציות חיפוש והחלפה ב־Arduino:

```arduino
String myString = "Hello World";
myString.replace("World", "Arduino");
Serial.println(myString); // פלט: Hello Arduino
```

```arduino
String myString = "This is a test string";
if (myString.indexOf("test") >= 0) { // אתר מיקום של המחרוזת "test"
  myString.replace("test", "example");
}
Serial.println(myString); // פלט: This is a example string
```

כדי ללמוד עוד על פונקציות חיפוש והחלפה, תוכלו לעיין בתיעוד של ארדואינו ולהתנסות עם דוגמאות נוספות.

## מעמקים נמוכים:

כעת, אתם בנוים עם פונקציות חיפוש והחלפה ב־Arduino, נוכל לעבור לשימושים יותר מתקדמים כדי לעשות דברים נוספים עם