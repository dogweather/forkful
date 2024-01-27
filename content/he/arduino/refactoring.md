---
title:                "רפקטורינג"
date:                  2024-01-26T01:18:20.376624-07:00
model:                 gpt-4-0125-preview
simple_title:         "רפקטורינג"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/refactoring.md"
---

{{< edit_this_page >}}

## מה ולמה?
ריפקטורינג הוא התהליך של שינוי הקוד שלך כדי לשפר את מבנהו ואת קריאותו מבלי לשנות את התנהגותו או את פונקציונליותו החיצונית. מתכנתים מבצעים ריפקטורינג כדי להפוך את הקוד שלהם לנקי יותר, קל יותר להבנה וקל יותר לתחזוקה, מה שבטווח הארוך הופך את ניפוי השגיאות והוספת תכונות חדשות לפחות מכאיבים.

## איך לבצע:

נניח שיש לך פונקציה בארדואינו שלך שעושה יותר מדי, כמו זו:

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  // פונקציה שעושה יותר מדי
  handleEverything();
}

void handleEverything() {
  // קריאת נתוני חיישן
  int sensorValue = analogRead(A0);
  // עיבוד נתוני החיישן
  sensorValue = map(sensorValue, 0, 1023, 0, 255);
  // הדפסת נתוני החיישן
  Serial.println(sensorValue);
  delay(500);
}
```

תהליך הריפקטורינג עשוי להיראות כמו פיצול `handleEverything()` לפונקציות קטנות וממוקדות יותר:

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  int sensorValue = readSensorData();
  int processedValue = processSensorData(sensorValue);
  printData(processedValue);
  delay(500);
}

int readSensorData() {
  return analogRead(A0);
}

int processSensorData(int sensorValue) {
  return map(sensorValue, 0, 1023, 0, 255);
}

void printData(int data) {
  Serial.println(data);
}
```

לאחר הריפקטורינג, הפונקציה `loop()` נהיית יותר קריאה, וכל משימה מטופלת על ידי פונקציה מוקדשת, מה שהופך את הקוד לקל יותר לניהול.

## צלילה עמוקה
בהיסטוריה, הריפקטורינג הפך לפופולרי עם עליית מתודולוגיות Agile ו-Test-Driven Development (TDD), שמתבססות על שיפור קוד קבוע כדי להתאים לדרישות משתנות. ישנם כלים ואסטרטגיות שונות לריפקטורינג - כמו טכניקת "Extract Method" שהשתמשנו בה בדוגמת הארדואינו שלנו. זה הכרחי כאשר אתה עובר מפרוטוטיפ מהיר לפרויקט יציב, שבו קריאות הקוד ותחזוקתו הופכות לקריטיות.

בעת ביצוע ריפקטורינג, חשוב להחזיק מערך טוב של בדיקות כדי לוודא שהשינויים לא הכניסו באגים. בעולם הארדואינו, בדיקות אוטומטיות לא תמיד הן פשוטות בגלל תלות בחומרה, אך ניתן עדיין להשתמש בבדיקות יחידה עבור חלקים של לוגיקה טהורה או להשתמש בסימולטורים.

אלטרנטיבות לריפקטורינג ידני כוללות שימוש בכלים מוקדשים לריפקטורינג, שמאטימים את הזיהוי של "ניחוחות קוד" ומציעים שינויים. עם זאת, לעיתים קרובות לכלים אלה חסרה העדינות עבור קוד למיקרו-בקרים וייתכן שלא יהיו זמינים בסביבת הפיתוח של ארדואינו.

בסופו של דבר, ריפקטורינג הוא אמנות שמאזנת בין שיפור מבנה הקוד הפנימי לבין הסיכון להכנסת פגמים. זה דורש ממך לחשוב על פרטי היישום כמו שימוש בזיכרון וזמן מעבד, במיוחד בגלל האופי המוגבל משאבים של מיקרו-בקרים.

## ראה גם
ניתן לחקור עוד על ריפקטורינג עם הספר המהותי של מרטין פאולר *ריפקטורינג: שיפור עיצוב הקוד הקיים*. להצצה קרובה יותר על תרגולים ספציפיים לארדואינו, בדוק את פורומים וקהילות הפיתוח של ארדואינו:

- [פורום ארדואינו - שאלות תכנות](https://forum.arduino.cc/index.php?board=4.0)
- [גורו ריפקטורינג](https://refactoring.guru/refactoring)

זכור, המטרה היא קוד נקי ומובן שאתה העתידי, ואחרים, יודו לך עליו. המשך להאק, ושמור על סדר!