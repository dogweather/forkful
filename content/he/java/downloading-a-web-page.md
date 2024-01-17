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

## מה ולמה?
להוריד דף אינטרנט היא פעולה שמתבצעת על ידי מתכנתים כדי לקבל את התוכן של דף אינטרנט ולשתף אותו באפליקציות שונות. זה מאפשר למתכנתים ליצור אפליקציות חכמות שיכולות לשלב תוכן מרשת האינטרנט ולספק ערכה מוסיפה למשתמש.

## איך לעשות זאת?
להלן דוגמאות לקוד ולפלט של הורדת דף אינטרנט בJava:
```
// ייבוא הספריות הנדרשות
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;

public class DownloadWebPage {

    public static void main(String[] args) throws IOException {

        // הגדרת הכתובת ה-URL של הדף שנרצה להוריד
        URL url = new URL("https://www.example.com");

        // פתיחת חיבור לאתר
        BufferedReader reader = new BufferedReader(new InputStreamReader(url.openStream()));

        // קריאה והדפסת כל שורה בדף
        String line;
        while ((line = reader.readLine()) != null) {
            System.out.println(line);
        }

        // סגירת החיבור
        reader.close();
    }
}
```
#### פלט:
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
    
    <!-- קובץ סגנון חיצוני -->
    <link rel="stylesheet" type="text/css" href="/css/style.css" />
</head>

<body>
    <div>
        <h1>Example Domain</h1>
    </div>
    
    <p>This domain is for use in illustrative examples in documents. 
    You may use this domain in literature without prior coordination or asking for permission.</p>
    
    </form>
</body>
</html>
```

## מעמקים
- היסטורית התפתחות של דרכים להוריד דפי אינטרנט.
- אפשרויות אחרות לקבלת תוכן מרשת האינטרנט, כגון ספריות של צד שלישי.
- פרטים טכניים נוספים כמו כתובת ה-URL, כמה מידע ניתן לקבל וכיצד לעקוב אחר תשובות שגויות.

## ראה גם
- [מדריך להורדת דף אינטרנט בשפת פייתון](https://realpython.com/python-web-scraping-practical-introduction/)
- [ספריה מוכנה לגירסת Java להורדת דף אינטרנט](https://jsoup.org/)