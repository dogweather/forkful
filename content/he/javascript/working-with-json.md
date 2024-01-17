---
title:                "עבודה עם פרמטר json"
html_title:           "Javascript: עבודה עם פרמטר json"
simple_title:         "עבודה עם פרמטר json"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/working-with-json.md"
---

{{< edit_this_page >}}

# מה ולמה?
עבודה עם JSON היא חלק בלתי נפרד מתכנות רשתות ואינה מדורגת כמבולגנת כמו ההדפסים שבשפות אחרות. זהו פורמט נתונים פשוט וקריא המשמש להעברת מידע בין מכשירים שונים, לרבות דפי אינטרנט ואפליקציות סלולריות. התמיכה הרחבה ב-JavaScript והגמישות הגדולה שלו בפרסום וקריאת נתונים, הופכים את JSON לפופולרי מבחינת מתכנתים.

# איך לעשות זאת:
מאגר נתונים בפורמט JSON מתחבר לסביבת התכנות של JavaScript כדי ליצור נתונים עם מבנה דומה למערך. נוכל ליצור משתנים משולשים ואפילו מערכים משולשים תוך שימוש בתווים כפולים (" "). לדוגמה: 
```Javascript
var person = {
  name: "רויטל",
  age: 28,
  interests: ["תכנות", "טיולים", "ריקוד"]
};
```
נוכל ליצור נתונים מכל סוג שנרצה ולהכניס אותם למאגר באופן פשוט וקריא.

לקרוא נתונים כאלה, נוכל להשתמש בפונקציית JSON.parse על מנת להמיר את הנתונים מפורמט JSON לאובייקט של JS. לדוגמה:
```Javascript
var person = '{"name": "רויטל", "age": 28, "interests": ["תכנות", "טיולים", "ריקוד"]}';
var parsed = JSON.parse(person);
console.log(parsed.name); // יצג את הערך "רויטל" בסטודנטים. 
```

# עיון מעמיק:
לפני JSON, רשתות היכרו עם מבנה נתונים כגון XML שהידוע מתכנני רשתות. עם זאת, החסרונות של XML כמו פערי מתן וכבדות, הביאו לפיתוח של JSON בשנת 2001. מאז, הוא כיכב בסביבת התכנות כפורמט נתונים מתוכנן, פשוט ואמין.

כאלטרנטיבה לתכנות ב-JavaScript ישנם מספר המחלפים כמו: XML, CSV ו- YAML. עדיין, עם כל שיפור ואחסון נתונים, כינוי המביא הוא תכנון וגם במקרי נתונים תקמה. (ASA) טלפאזת מודרנית ו- JSON מנסהו של והאחת הביצוע של שיפור/

# ראה גם:
1. עורך טקסט חינם עבור JSON: https://jsoneditoronline.org/
2. תיעוד מלא על עבודה עם JSON ב-JavaScript: https://www.w3schools.com/js/js_json_intro.asp
3. כלי חינם ליצירת מערכי JSON: https://json-generator.com/