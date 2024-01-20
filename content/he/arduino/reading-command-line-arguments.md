---
title:                "קריאה של ארגומנטים משורת הפקודה"
html_title:           "C#: קריאה של ארגומנטים משורת הפקודה"
simple_title:         "קריאה של ארגומנטים משורת הפקודה"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## מה ולמה?
קריאת ארגומנטים משורת הפקודה היא שיטת הנהגה בה התוכנית שלנו מקבלת מידע מהמשתמש בעת הפעלת התוכנית. זה מאפשר לנו להפוך את התוכנית שלנו לגמישה והתאמה אישית יותר.

## איך לעשות:
הנה דוגמה לאיך לקבל ארגומנטים משורת הפקודה באמצעות Arduino. 
```Arduino
int argCount;
char **args;

void setup() {
  Serial.begin(9600);
  argCount = getArguments();
  
  for (int i=0; i<argCount; i++) {
   Serial.println(args[i]);
 }
}

int getArguments() {
   // כאן אנחנו מטפלים בארגומנטים, חזרה למספר הארגומנטים שהוזנו
   // נשים לב שלא הצגנו דוגמה מלאה
}
```

## שקיעה עמוקה:
עם השנים, הייתה שימוש רחב של שורת פקודה בכל מיני שפות תוכנה - מ-C ו-Java ל-Python ו-Ruby. בארדוינו, אנחנו בעיקר משתמשים בכלי זה בקשר לתקשורת סידורית.

אלטרנטיבות לקריאת ארגומנטים משורת הפקודה כוללות בניית ממשק משתמש גרפי או מנהל קבצים עם הגדרות מובנות. בנוסף, אפשר להשתמש ב-APIs כמו EEPROM על מנת לשמור מידע בין הופעות שונות של התוכנית.

בסביבת ארדוינו, קריאת ארגומנטים משורת הפקודה היא קצת יותר מורכבת מאשר בשפות תוכנה אחרות, אבל היא מאפשרת שליטה מרבית בקוד שלך ומציעה גמישות נוספת.

## נושאים קשורים:
1. קריאת וכתיבה של EEPROM בארדוינו:
<https://www.arduino.cc/en/Tutorial/LibraryExamples/EEPROMWrite>

2. שימוש בשורת פקודה בחלונות עם ארדוינו:
<https://github.com/arduino/Command-Line>