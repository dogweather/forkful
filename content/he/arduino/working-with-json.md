---
title:                "עבודה עם JSON"
html_title:           "Arduino: עבודה עם JSON"
simple_title:         "עבודה עם JSON"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## למה

כאשר מתכנתים משתמשים בפלטפורמת ארדוינו, הם נתקלים לעתים קרובות בנתונים מבניים בפורמט JSON. ההבדל מבין ארדוינו ושפות תכנות אחרות הוא שגישת JSON לנתונים נוחה יותר עבור ארדוינו וניתן ליישם אותם בצורה יעילה עם כמה שורות קוד בלבד. לכו נראה איך!

## איך לעשות זאת

כאשר עובדים עם JSON בארדוינו, ניתן לפענח, ליצור ולשמור נתונים בהתאם לצרכים המיוחדים שלכם. הנה דוגמה לקוד פשוט לפענוח נתוני JSON ממקור חיצוני:

```Arduino
#include <ArduinoJson.h>
 
String jsonData = "{\"name\":\"John\", \"age\":20}";
StaticJsonDocument<200> doc;
deserializeJson(doc, jsonData);
String name = doc["name"];
int age = doc["age"];
```

בדוגמה הזו, אנחנו מגדירים נתוני JSON למשתנה, מפענחים אותם עם ספריית ArduinoJson ולאחר מכן משתמשים בערכי המשתנים המפוענחים כדי לגשת ולעבד את הנתונים. פשוט ויעיל!

## לכיוון העומק

עכשיו עם היכרות קצרה עם הפורמט ועם הצלחה ביעילות של אותם נתונים בפלטפורמה קטנה כמו Arduino, כדאי לקחת את הבנת הJSON שלכם שלב אחד קדימה. הנה כמה טיפים לעבודה עם JSON על ארדוינו:

- שימו לב כי לפעמים יהיו צורת JSON נתונים באורך משתנה ויהיה עליכם להתאים את גודל הביטים בכדי לתת מקום מספיק עבורם.
- מפענחי נתונים כמו [ArduinoJson][1] מאפשרים לנו להשתמש בביטים גם כדי לחודד את הדרך שהנתונים מצויים בה וגם כדי למפענח דווקא את אותם נתוני JSON ממקור חיצוני.
- אם במ