---
title:                "שליחת בקשת http"
html_title:           "Java: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

מה ולמה?

שלושת המלים הללו - "שליחת בקשת HTTP" נשמעות כמו משהו מורכב וקשה, אבל בעצם זה פשוט מאוד. כשברשותנו יש צורך לגשת ולקבל מידע מאתר אינטרנט, כמו למשל לשלוח פורמטים עבור הרשמה או לקבל תוצאות מחיפוש גוגל, אנחנו משתמשים בבקשת HTTP. זהו דרך של "לדבר" עם האתר הזה ולבקש ממנו מידע מסוים. מתוך כך בקשת HTTP מאפשרת למתכנתים ליצור אפליקציות מתקדמות יותר וליישם פונקציות מיוחדות על מספר אתרים שונים.

איך לעשות זאת?

```java
import java.net.HttpURLConnection;
import java.net.URL;
import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;

URL url = new URL("https://www.example.com"); //כתובת האתר שבו אנחנו רוצים לשלוח בקשת HTTP
HttpURLConnection con = (HttpURLConnection) url.openConnection(); //פתיחת חיבור עם האתר
con.setRequestMethod("GET"); //קביעת שיטת הבקשה כמו GET, POST או PUT כמו שרוצים
InputStream is = con.getInputStream(); //הקבלת המידע מהאתר בסוג הטקסט המתאים
BufferedReader br = new BufferedReader(new InputStreamReader(is));
String line;
StringBuilder response = new StringBuilder();
while ((line = br.readLine()) != null) { //קריאת המידע בקוד ושמירתו בתוך משתנה
    response.append(line);
}
br.close();
System.out.println(response.toString()); //הדפסת התוצאה בקונסול
```

כאן נראה דוגמה של כיצד לשלוח בקשת HTTP באמצעות קוד Java. נשתמש בספריות מובנות כדי לפתוח חיבור עם האתר הרלוונטי ולקבל את המידע שבו. כמו בדוגמה לעיל, נשתמש בבקשת HTTP מסוג GET בכדי לקבל את התוכן של האתר. כמו כן, נדפיס את התוצאה בקונסול כדי לוודא שהכל עבד כראוי.

עכשיו נסתכל על מה באמת קורה מאחורי הקלעים ביצירת בקשת HTTP.

מעמק נראה דרך ותולדות:

מתעלם מכך שתחת כל יישום רשת שאתה משתמש בתוכנית שלך, יש פעולות רשת ברקודאונים. רקודאונים זהו דרך בה תוכנית העובד העם (או יותר מדוייקת הפרוטוקול של רשת ניידת) מתחברת לשרת ברשת ברקודאונים הנו פתח חזית העבודה הנמיכה שדרכו היינו מחברים באינטרנט.

נראה לדוגמה דרך מובן כדי לתקשור עם האתר בדיוק עם שתי ויזעוי רשת אכטו אומת ברקודאונים אחורי רבינות אתרי איגרות. הפתיחת רשת ברקודאנית עם אתר אכטו אומת עוזב את הפרוטוקול של Nieve או מתחבר עם כל הנותני שירות שקיימים עודפים שלא-עהתרים ה-2008RON / UFLUX או ה-	controller之外עשתיות.

ראה גם:

- https://www.w3.org/Protocols/HTTP
- https://developer.mozilla.org/en-US