---
title:                "ניתוח html"
html_title:           "Arduino: ניתוח html"
simple_title:         "ניתוח html"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## מדוע

זהו אמנם נושא חשוב ומעניין, אך רבים מפחדים מהמונח "HTML". למה ללמוד לפענח את השפה הזו במקום להשתמש בכלי כמו כרטיסייה, כשייתכן שטכנולוגיית "Arduino" תתאים יותר להם? הסיבה העיקרית לכך היא שכמעט כל מכשיר אלקטרוני בתוך הבית או במשרד מתקשר עם האינטרנט. לכן מכשירים אלו תומכים בתקשורת סדרתית, ובשפת תכנות כמו HTML, שמתארת את החלקים של מסמכים. על ידי הלמידה לפענח את השפה הזו, ניתן לתכנת מכשירים שיכולים לקרוא, לשלוח ולכתוב מידע HTML.

## איך לעשות זאת

כדי להתחיל לפענח HTML באמצעות "Arduino", אנו צריכים להשתמש בסירטוני המחשב כדי לכתוב תוכניות. נחשב, לדוגמה, לעצם "Arduino"​​שמגיש כרטיסייה " Ethernet "שמאפשרת תקשורת עם הרשת המקוונת תוך הסתמכות על פרוטוקול תקשורת כגון IP TCP סדרתי. על מנת להתחיל בשפת HTML, נשתמש בספריית "Http" המשמשת את יישום התכנית שלנו ותעודת אימות בתוכנית.

```Arduino
#include <SPI.h>
#include <Ethernet.h>
#include <HTTPLib.h>

void setup() {
  // Initialize Ethernet library
  Ethernet.begin(mac);
  // Start serial communication
  Serial.begin(9600);
}

void loop() {
  // Make HTTP request
  // HTML parsing
  // Print results to serial monitor
  Serial.println("Parsed HTML data");
}
```

התוכנית שלנו תדפיס את המידע שנפענח מהדפים המייצגים את המידע המועבר על ידי פרוטוקול התקשורת "IP TCP" לחברה המיוחדת של "Ethernet Shield". ניתן לעבור על מה שקראנו, לקפוץ עלתוכניות על כרטיסייה "IP"×"TCP".

## עיון מעמיק

כמו בשפות כמו "C++", השפת HTML משתמשת