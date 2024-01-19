---
title:                "הורדת דף אינטרנט"
html_title:           "C++: הורדת דף אינטרנט"
simple_title:         "הורדת דף אינטרנט"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## מה ולמה?
הורדת דף אינטרנט היא התהליך שבו אנו מקבלים את הנתונים של דף אינטרנט או HTML מהשרת. מתכנתים עשויים לרצות לעצב מחדש את הנתונים לשימושים משלהם, לדוג, עיבוד מידע, ניתוח דאטה, בדיקות אוטומטיות.

## איך לעשות:
באמצעות הספרייה `java.net` של Java, אנו יכולים להוריד דף אינטרנט כך:

**קוד דוגמה**
```Java
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;

public class Main {
    public static void main(String[] args) throws Exception {

        URL url = new URL("http://www.example.com");
        URLConnection conn = url.openConnection();
        BufferedReader br = new BufferedReader(new InputStreamReader(conn.getInputStream()));

        String line;
        while ((line = br.readLine()) != null)
            System.out.println(line);
            
        br.close();
    }
}
```
הקוד מעלה יוציא את כל ה HTML של דף האינטרנט www.example.com. 

## טיול עמוק
1. **הקשר היסטורי**: בעבר, לפני המבוסס-אינטרנט, היו צורכים לקבל נתונים משרתים באופן מדובר עם פרוטוקולים מיוחדים. היום, אנו משתמשים ב HTTP ו- HTTPS, ואפילו תכנות מאוד מוכרות כמו לוחות יד-שנייה או IMDb משתמשות באותה טכניקה שאנו למדנו כאן.
 
2. **אלטרנטיבות**: ישנן ספריות אחרות כמו Jsoup או HttpClient שיכולות להקל עלינו את החיים עם כמה פיצ'רים מגניבים, אך באופן כללי, הן משתמשות באותה הטכניקה שהוצגה למעלה.

3. **פרטים על המימוש**: כאשר אנו מבצעים את החיבור אל המען URL, ואז קוראים מהזרם הקלט - אנו בעצם מקבלים את הנתונים בצורת מחרוזת String של HTML. בנקודה זו, אנו יכולים לעבוד עם הנתונים הללו כך שנראה לנחות לנו.

## לקריאה נוספת:
- [Java SE Documentation](https://docs.oracle.com/en/java/javase/index.html)
- [Java Networking Tutorial](https://www.tutorialspoint.com/java/java_networking.htm)
- [Jsoup, a Java library for working with real-world HTML](https://jsoup.org/)