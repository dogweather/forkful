---
title:                "פענוח HTML"
html_title:           "Java: פענוח HTML"
simple_title:         "פענוח HTML"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/parsing-html.md"
---

{{< edit_this_page >}}

מה זה parsing HTML ולמה מתכנתים עושים את זה?

הפרסום של HTML הוא תהליך של קריאת קובץ HTML והמרה שלו למבנה נתונים מובהק בשפה שהוא קשור אליה, כמו למשל Java. זה מאפשר למתכנתים לקרוא ולעבוד עם נתונים באופן מבוסס של HTML, כך שהם יכולים לעצב וליצור אפליקציות ואתרי אינטרנט.

איך לעשות זאת?

הנה דוגמא פשוטה של פריסת HTML עם שימוש בקוד Java:

```
// ייבוא מחלקות נדרשות
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;

public class HTMLParser {

   public static void main(String[] args) throws IOException {

      // יצירת קישור לכתובת אתר
      URL url = new URL("https://www.example.com/");
      
      // קריאת קובץ HTML עם שימוש בפעולת BufferedReader
      BufferedReader br = new BufferedReader(new InputStreamReader(url.openStream()));
      String line;
      
      // קריאת כל קו בקובץ והדפסתו למסך
      while ((line = br.readLine()) != null) {
         System.out.println(line);
      }
      
      // סגירת BufferedReader
      br.close();
   }
}
```

כתוצאה מהקוד לעיל, נוכל לקבל את התוכן המלא של האתר שלנו ולעבוד עם הנתונים המכילים אותו.

עומק נוסף

עומק נוסף של פריסת HTML כולל היסטוריה ואלטרנטיבות, עם מידע נוסף על איך זה עובד ואיך להפעיל את הקוד.

ראו גם

כדי לקבל מידע נוסף על השתמשות בפריסת HTML בשפת Java, כדאי לקרוא את המאמר הבא: https://www.baeldung.com/java-html-parsing.