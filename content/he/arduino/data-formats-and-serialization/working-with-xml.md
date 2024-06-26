---
date: 2024-01-26 04:27:53.684122-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05E0\u05E9\u05EA\
  \u05DE\u05E9 \u05D1\u05E1\u05E4\u05E8\u05D9\u05D9\u05EA `XMLWriter` \u05DB\u05D3\
  \u05D9 \u05DC\u05D9\u05E6\u05D5\u05E8 XML \u05D5\u05D1\u05E1\u05E4\u05E8\u05D9\u05D9\
  \u05EA `tinyxml2` \u05DB\u05D3\u05D9 \u05DC\u05E4\u05E2\u05E0\u05D7 \u05D0\u05D5\
  \u05EA\u05D5. \u05D4\u05EA\u05E7\u05D9\u05E0\u05D5 \u05E7\u05D5\u05D3\u05DD \u05D0\
  \u05EA \u05D4\u05E1\u05E4\u05E8\u05D9\u05D5\u05EA \u05D3\u05E8\u05DA \u05DE\u05E0\
  \u05D4\u05DC \u05D4\u05E1\u05E4\u05E8\u05D9\u05D5\u05EA \u05D1\u05E1\u05D1\u05D9\
  \u05D1\u05EA \u05D4\u05E4\u05D9\u05EA\u05D5\u05D7 \u05E9\u05DC \u05D0\u05E8\u05D3\
  \u05D5\u05D0\u05D9\u05E0\u05D5.\u2026"
lastmod: '2024-03-13T22:44:39.806543-06:00'
model: gpt-4-0125-preview
summary: "\u05E0\u05E9\u05EA\u05DE\u05E9 \u05D1\u05E1\u05E4\u05E8\u05D9\u05D9\u05EA\
  \ `XMLWriter` \u05DB\u05D3\u05D9 \u05DC\u05D9\u05E6\u05D5\u05E8 XML \u05D5\u05D1\
  \u05E1\u05E4\u05E8\u05D9\u05D9\u05EA `tinyxml2` \u05DB\u05D3\u05D9 \u05DC\u05E4\u05E2\
  \u05E0\u05D7 \u05D0\u05D5\u05EA\u05D5."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML"
weight: 40
---

## איך לעשות:
נשתמש בספריית `XMLWriter` כדי ליצור XML ובספריית `tinyxml2` כדי לפענח אותו. התקינו קודם את הספריות דרך מנהל הספריות בסביבת הפיתוח של ארדואינו.

יצירת מסמך XML:

```Arduino
#include <XMLWriter.h>

void setup() {
  Serial.begin(9600);
  
  XMLWriter xml(&Serial); // משתמשים ב-Serial לפלט
  
  xml.header();
  xml.tag("greeting").tag("text").text("Hello, world!").close().close();
  xml.flush();
}

void loop() {
}
```

פיענוח מחרוזת XML:

```Arduino
#include <tinyxml2.h>

tinyxml2::XMLDocument doc;
doc.Parse("<greeting><text>Hello, world!</text></greeting>");

tinyxml2::XMLElement* text = doc.FirstChildElement("greeting")->FirstChildElement("text");
if (text != nullptr) {
  Serial.println(text->GetText());
}
```

דוגמת פלט:

```
<greeting>
  <text>Hello, world!</text>
</greeting>
```

## עיון נוסף
XML, או שפת הסימון הניתנת להרחבה, היא שפת סימון המגדירה קבוצת כללים לקידוד מסמכים בפורמט שהוא גם קריא לאדם וגם ניתן לקריאה על ידי מכונה. היא קיימת מאז סוף שנות ה-90 ונמצאת בשימוש נרחב בתחומים שונים, במיוחד שם שנדרשת החלפת נתונים בלתי תלויה בפלטפורמה. משאבי הזיכרון המוגבלים של ארדואינו הופכים את העבודה עם XML למאתגרת יותר מאשר על מחשב אישי. לכן, ספריות קלות משקל הן קריטיות. למרות ש-JSON זכה לפופולריות לשם החלפת נתונים בשל תחבירו הפשוט יותר וטביעת הרגל הקטנה יותר שלו, XML עדיין בשימוש נרחב, במיוחד כאשר מתמודדים עם מערכות ישנות או יישומים הדורשים אימות מסמך באמצעות סכימות. חשוב ליישום XML בארדואינו הוא פרסום אינטראקטיבי, הקורא את המסמך בקטעים כדי לשמור על שימוש נמוך בזיכרון.

## ראו גם
- [תיעוד ספריית TinyXML-2](https://leethomason.github.io/tinyxml2/)
- [ספריית Arduino JSON](https://arduinojson.org/) כאלטרנטיבה בעבודה עם נתוני JSON.
- [מדריך XML של W3Schools](https://www.w3schools.com/xml/) ללמידה כללית על XML.
- [מפרט XML של W3C](https://www.w3.org/XML/) לתקנים והמלצות XML הרשמיים.
