---
title:                "הורדת עמוד אינטרנט"
html_title:           "Java: הורדת עמוד אינטרנט"
simple_title:         "הורדת עמוד אינטרנט"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## למה

כשאנו גולשים באינטרנט, אנחנו מבקשים לקבל מידע על מה שאנחנו מחפשים. הורדת דף אינטרנט עוזרת לנו לקבל את המידע הזה בצורה מהירה ומאורגנת יותר. 

## איך לעשות זאת

ראשית, נצטרך ליצור אובייקט מסוג `URL` עם הכתובת של הדף שאנחנו רוצים להוריד. לדוגמה: 

```Java
URL url = new URL("https://www.example.com");
```

לאחר מכן, נצטרך ליצור חיבור עם השרת של האתר באמצעות האובייקט `URLConnection` ולקבל קוד תגובה מהשרת: 

```Java
URLConnection connection = url.openConnection();
int responseCode = connection.getResponseCode();
```

אם הקוד תגובה הוא 200, כלומר הצלחנו להתחבר לאתר, נצטרך לקרוא את מכפלת הקלט מהאתר באמצעות `BufferedReader` ולהדפיס את הנתונים על המסך: 

```Java
if (responseCode == 200) {
    BufferedReader in = new BufferedReader(new InputStreamReader(connection.getInputStream()));
    String inputLine;
    while ((inputLine = in.readLine()) != null) {
        System.out.println(inputLine);
    }
    in.close();
}
```

כאן אנו משתמשים בלולאת `while` כדי לקרוא כל שורה ממכפלת הקלט ולהדפיס אותה על המסך. 

## חפירה עמוקה

הפעולה של הורדת דף אינטרנט היא לא פשוטה כמו נראה. כל מכשול אינטרנטי יכול לפגוע בתהליך - מהירות אינטרנט איטייה, שגיאות בשרת, אתרים מאובטחים ועוד. ניתן לפתור מכשולים אלה על ידי שימוש בטכניקות כמו טיומאאוט, בדיקת תקינות ויישום מנגנוני בטיחות מתקדמים. 

## ראה גם

- מדריך ללימוד Java באתר W3Schools: https://www.w3schools.com/java/
- התיעוד המפורט של Java למתכנתים: https://docs.oracle.com/javase/tutorial/
- פורום Java באתר Stack Overflow: https://stackoverflow.com/questions/tagged/java