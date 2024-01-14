---
title:                "Arduino: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## למה: 

יתרון בשליחת בקשת HTTP עם אימות בסיסי באמצעות ארדואינו.

## איך לעשות זאת:

המתחילים בתכנות עם ארדואינו כנראה מכירים את הפעולות הבסיסיות כגון יצירת קשר עם תקליטן וקבלת ערכים מחיישנים. אבל ידע לשלוח בקשות HTTP תוך שימוש בתכנות אפשרי רק על ידי הוספת בבקשה את שכבת האימות. זה נדרש כאשר מעונינים לקבל עדכונים מאתרים חיצוניים, לדוגמה לקבלת מידע מתחנת מזג אוויר או חשבונית חשמלית.

``` arduino
#include <WiFiClient.h>
```

שימו לב כי עלינו את הספריה 'WiFiClient' כדי לאפשר גישה לאינטרנט דרך שרת ה-WiFi שלנו. בצעו את הקישור לרשת עם הנתונים הרלוונטיים והתחברו לשרת:

```arduino
char ssid[] = "NameOfNetwork"; // שם הרשת
char pass[] = "NetworkPassword"; // סיסמת רשת
int status = WL_IDLE_STATUS;

WiFi.begin(ssid, pass); // נסו להתחבר לרשת
delay(1000); // המתינו כמה שניות עד להתחברות לרשת

if (WiFi.status() == WL_CONNECTED) {
  Serial.println("Connected!");
}
```

אחרי התחברות לרשת מגלים את כתובת ה-IP של שרת האתר שמעוניין לקבל עדכונים ממנו וגם הנתונים לאימות בבקשה:

```arduino
IPAddress server(192,168,1,1); // כתובת ה-IP של השרת
int port = 80; // המספר של הפורט המשתמש ע"י השרת

String auth = "username:password"; //פרטי האימות כפי שקיבלתם מהאתר
```

כעת אנו משתמשים בפונקציה "requestBasicAuthentication" עם הערך המתאים של אימות ונשלח את הבקשה לאתר:

```arduino
if (client.connected()) {
  client.print(requestBasicAuthentication("GET", "/", server));
} else {
  Serial.println("Connection failed!");
}
```

סוג הבקשה שמחכה לקבלה מאתר הוא "GET", אבל אפשר להשתמש בסוגים אחרים כמו "POST" א