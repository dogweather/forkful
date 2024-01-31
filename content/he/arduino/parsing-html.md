---
title:                "ניתוח HTML"
date:                  2024-01-20T15:30:13.246657-07:00
html_title:           "Arduino: ניתוח HTML"
simple_title:         "ניתוח HTML"

category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## מה ולמה?
פיענוח HTML הוא תהליך שבו ניתוח קוד HTML והוצאת נתונים רלוונטיים ממנו. תוכניתנים עושים זאת כדי לאסוף מידע אוטומטית מאתרי אינטרנט לשימושים שונים, כגון סינכרון נתונים או מוניטורינג של תוכן.

## איך לעשות:
הרשמה לשימוש בחומרה וספריות נדרשות בראש הקוד:
```Arduino
#include <Ethernet.h>
#include <SPI.h>
```

קישור לרשת וביצוע בקשה HTTP:
```Arduino
EthernetClient client;
client.connect(server, 80);
client.println("GET /path/to/resource HTTP/1.1");
```

קריאה ופיענוח תגי HTML:
```Arduino
if (client.find("<title>")) {
  String title = client.readStringUntil('<');
  Serial.println(title);
}
```

פלט דוגמא:
```
Arduino Project Page
```

## צלילה לעומק:
במקור, פיענוח HTML נעשה יידנית עם ספריות PHP, Python או כל שפה סקריפטית. לאורך הזמן, פותחו כלים ייעודיים כמו BeautifulSoup ו-lxml. התמיכה של ארדואינו בפיענוח HTML מוגבלת בשל חומרה לא חזקה. במקום לנתח HTML מורכב, רבים משתמשים בארדואינו לדרוש מהשרת מידע כבר מעובד בפורמט JSON או XML דרך API.

## ראו גם:
- [Arduino Ethernet Library](https://www.arduino.cc/en/Reference/Ethernet)
- [Arduino JSON parsing with ArduinoJson library](https://arduinojson.org/)
- [Web Scraping with Python](https://realpython.com/beautiful-soup-web-scraper-python/)
