---
title:                "Arduino: עובדים עם JSON"
simple_title:         "עובדים עם JSON"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## למה

קודם כל, מה זה פרוטוקול JSON? JSON הוא פורמט לתיאור נתונים המשמש בעיקר לשיטות תקשורת בין שרת לצד לקוח. לדוגמה, JSON נמצא בשפת אתרים (HTML) ותוכנות מגדירות לבצע פעולות אופציונליות כמו השגת מידע והעברת מידע ממכשיר לצד שרת. ברגע שמכירים בפרוטוקול זה, ניתן לנצל אותו בקוד ארדואינו שלנו וליצור חיבורים מגוונים ומעניינים למכשירים אחרים.

## איך

כדי להתחיל לעבוד עם פרוטוקול JSON בארדואינו, יש להוריד ולהתקין את הספרייה "ArduinoJSON". לאחר מכן, ניתן לבנות צורת נתונים עם שדות מסוימים ולהמיר אותה לפורמט JSON. לדוגמה, ננסה לחבר את המכשיר שלנו לרשת האינטרנט ולשלוח נתונים לשרת. לפני הכל, נצטרך לכתוב את השרת והנתב, ולאחר מכן נשתמש בספרייה כדי ליצור את צורת הנתונים הרצויה, לצורך שליחת המידע לשרת.

```Arduino

#include <ArduinoJson.h>

void setup(){
  // הגדרת גם שרת ונתב כאן
}

void loop(){
  // יצירת צורת נתונים והמרתה לפורמט JSON
  DynamicJsonDocument doc(100); // נציג 100 בתי זיכרון זמינים לצורת הנתונים
  doc["sensor"] = "Thermometer";
  doc["value"] = 25.8;

  // שליחת הנתונים לשרת המתאימים
  // שליחה למספר הכתובת IP 192.168.0.100 בפועל
  client.print("POST /get_data HTTP/1.1\n");
  client.print("Host: 192.168.0.100\n");
  client.print("Content-Type: application/json\n");
  client.print("Content-Length: ");
  client.print(measureJson(doc));
  client.print("\n\n");
  serializeJson(doc, client);
}
```

כמו שאנו רואים בדוגמה, הספרייה מאפשרת לנו לבנות צורת נתונים ולהמיר אותה לפורמ