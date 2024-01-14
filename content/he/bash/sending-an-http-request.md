---
title:                "Bash: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

# מדוע
שלוש אפשרויות נפוצות לכתיבת קוד - פייתון, ג'אווה סקריפט ובאש. כל אופציה מציעה ממשק משתמש נוח כדי לשלוח בקשות HTTP. 

## איך לעשות זאת
```Bash
# מתקינים את חבילת curl
sudo apt-get install curl

# שליחה של בקשת GET
curl http://www.example.com

# שליחת בקשת POST עם נתונים מוצפנים כצורת JSON
curl -X POST -d '{"name": "John", "age": 30}' http://www.example.com
```

## מקדמות בשליחת בקשות HTTP
כדי להבין את מנגנון השליחה של בקשות HTTP, כדאי להיות מעוניינים במקדמות כמו כתיבת קוד באש, פענוב פרמטרים כרגיל וקבלת מידע חזרה מהשרת אלייך. ניתן גם להיעזר בספריות גיסון כדי לעשות עיבוד פרמטרים ומיפוי אובייקטים לכן יתאים גישה הודו נעצלת (REST).

# ריאות עמוקות
מגוון רחב של ספריות עוזרות ניתנות להפעלה דרך בשליחת פניית אתר. ניתן להשתמש ב-CURL, פייתון עם דפדפן קליינט ועוד! ישנם מספר אפשרויות לכתיבת קוד עושים שליחת בקשות HTTP מדי יום קל יותר. :)

# ראה גם
- [בעברית: שליחת בקשות HTTP עם פייתון באמצעות הספרייה requests] (https://www.pythonforbeginners.com/requests/using-requests-in-python)
- [בעברית: המסמך הרשמי של באש באתר מפתחים של נטפליקס] (https://www.baeldung.com/httpclient-guide)
- [באנגלית: מדריך לשליחת בקשות HTTP דרך לינוקס ש#] (https://linuxize.com/post/curl-command-examples/)