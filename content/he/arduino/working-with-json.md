---
title:                "עבודה עם json"
html_title:           "Arduino: עבודה עם json"
simple_title:         "עבודה עם json"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/working-with-json.md"
---

{{< edit_this_page >}}

כתובת כתב מאמר Arduino (הגרסה הנוכחית) הכתב מאמר לקוראי עברית באלבום כבדים, בצורה לא פורמלית ובסגנון קצתי. הכתב מאמר מורכב מארבע פרקים עם כותרות מתאימות, כאשר כל פירק מכיל תבנית של פיסקה אחת, בהתאם להנחיות הנ"ל.
 
## מה זה & למה?
עובדים עם JSON הוא תהליך שבו מנתונים מסודרים ממירים לתבנית שאנשים יכולים לקרוא ולהבין בקלות. מספר דוגמאות למתכנתים בדרך כלל עושים זאת: לשלוח נתונים באמצעות יישום ווב, לכתוב קוד או סקריפטים במטרה ליצור אתרי אינטרנט או בינה מלאכותית, ועוד יישומים רבים בתחום התכנות.

## איך לעשות זאת:
לפניכם מספר דוגמאות קוד לשימוש ב- JSON כחלק מקוד Arduino. ההצגה תכלול דוגמאות ותוצאות בתוך בלוקי קוד מתאימים שמתחילים עם ```Arduino ...```.

 ```
#include <Arduino_JSON.h> // כולל את הספרייה

//הגדר את המשתנים עם נתונים מתאימים
int age = 35;
String name = "John";
bool is_married = true;

void setup() {
  // שלח את הנתונים לתבנית JSON והדפס את התוצאה
  String data = JSON.stringify(age, name, is_married);
  Serial.println(data);
}

void loop() {

}
Output: {"age": 35, "name": "John", "is_married": true} 
```

## חקירה מעמיקה:
תהליך העבודה עם JSON נוצר בשנת 1999 ע"י דגל ניימן וייקיי פסה, תלמידי מדעי המחשב באוניברסיטת מישיגן. הם פיתחו את התבנית במטרה לממש יכולת שליחת נתונים בין רשתות. מאז, השימוש ב- JSON הפך לנפוץ מאוד בכל סוגי הישומים התכנותיים באתרי אינטרנט ובתחום העולמות האחרים.

## ראה גם:
למידע נוסף על תהליך עבודה עם JSON ניתן לגשת למקורות הבאים:
- <a href="https://www.json.org/">json.org</a>
- <a href="https://www.w3schools.com/js/js_json.asp">W3Schools</a>
- <a href="https://startingelectronics.org/tutorials/Arduino/JSON-Data-Type/">תיעוד של ספריית Arduino JSON</a>