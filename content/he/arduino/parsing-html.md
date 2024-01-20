---
title:                "ניתוח HTML"
html_title:           "Arduino: ניתוח HTML"
simple_title:         "ניתוח HTML"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## מה זה ולמה?

עיבוד HTML הוא התהליך של קריאת קוד HTML והמרתו למבנה מידע שיכול להיות מנוהל על ידי תוכנה. אנו עובדים HTML כדי לאפשר לנו להבין את מידע האתר. 

## איך:

באמצעות Arduino Ethernet Library, נוכל לנתח HTML מאתרי אינטרנט. עם כמה שורות של קוד, אנו משיגים את זה:

```Arduino
#include <Ethernet.h>

EthernetClient client;

void setup() {
  Serial.begin(9600);
  
  if (Ethernet.begin(mac) == 0) {
    Serial.println("Failed to configure Ethernet using DHCP");
    for(;;)
      ;
  }
}

void loop() {
  if (client.connect(server, 80)) {
    client.println("GET / HTTP/1.0");
    client.println();
  }
  else {
    Serial.println("connection failed");
  }

  while(client.connected()) {
    if(client.available()) {
      char c = client.read();
      Serial.print(c);
    }
  }
  
  client.stop();
}
```

הקוד מחבר לשרת בפורט 80. הוא מבצע GET request ומדפיס את התשובה, הכוללת גם קוד HTML.

## הצצה למטה:

עיבוד HTML הוא כלי חיוני בעולם הרשת. חשוב לדעת שהשיטה שהצגנו היא דרך פשוטה ובסיסית שמשתמשת ב- Ethernet library של Arduino. למדנו גם אודות הטכניקה של GET Request. ישנם גם שיטות אלטרנטיביות, כמו ביבליות JavaScript או Python. 

## ראה גם:

- [W3 Schools - HTML Parsing](https://www.w3schools.com/php/php_ajax_rss_reader.asp)
- [Arduino Ethernet Library](https://www.arduino.cc/en/Reference/Ethernet)
- [Python - BeautifulSoup](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)