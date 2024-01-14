---
title:                "Java: הורדת עמוד אינטרנט"
simple_title:         "הורדת עמוד אינטרנט"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## למה

בכדי להיות מתכנת יעיל, ישנם רבים מאיתנו שזה מעניין ללמוד כיצד אתה יכול להוריד דף אינטרנט בקוד Java. ייתכן שיש לך כבר את האתגר לבנות אפליקציה המתבססת על מידע מקורי מדף אינטרנט, או שאתה פשוט רוצה להרחיב את יכולות התכנות שלך. בכל מקרה, לדעת כיצד להוריד דף אינטרנט בקוד Java יכול להיות כלי חשוב בתיק הכלים שלך כמתכנת.

## כיצד לעשות זאת

תהליך הורדת דף אינטרנט בקוד Java פשוט. כדי להתחיל, תצטרך להשתמש במחלקת `URL` כדי ליצור אובייקט שמייצג את הכתובת האינטרנט שאתה רוצה להוריד. לאחר מכן, תוכל למצוא ולפתוח חיבור לאתר עם `openConnection()` ולקרוא את המידע מרחוק באמצעות `InputStream`. לבסוף, תצטרך להפעיל את המידע שקיבלת באמצעות `BufferedReader`.

```Java
URL url = new URL("https://www.example.com");
URLConnection connection = url.openConnection();
InputStream stream = connection.getInputStream();
BufferedReader reader = new BufferedReader(new InputStreamReader(stream));
String line;
while ((line = reader.readLine()) != null) {
    System.out.println(line);
}
```

כשתפעיל את הקוד הנ"ל, הוא יוריד את כל התוכן של דף אינטרנט וידפיס אותו למסך. אתה יכול גם לשמור את התוכן בקובץ אם תרצה.

## נביחה עמוקה

המידע שהתקבל מפעולת הורדת הדף אינטרנט יהיה בפורמט של טקסט פשוט. זה יכול להיות מועיל אם אתה רוצה לאחזר נתונים מיישומים אחרים, או אם אתה רוצה לעשות עיבוד נוסף על המידע המראה. כמו כן, תוכל להשתמש בספריות נוספות כדי להפריד את המידע לשדות