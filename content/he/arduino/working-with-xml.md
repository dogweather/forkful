---
title:                "עבודה עם XML"
date:                  2024-01-26T04:27:53.684122-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם XML"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/working-with-xml.md"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם XML בארדואינו מערבת פיענוח וטיפול בנתוני XML, שבדרך כלל מגיעים מ-APIs של אינטרנט או קבצי תצורה. מתכנתים עושים זאת כדי לשלב עם שירותים המשתמשים ב-XML להחלפת נתונים או לאחסון נתונים בפורמט מובנה וקריא לאדם.

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
