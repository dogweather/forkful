---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "C: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה ולמה?
HTTP בקשה עם אימות בסיסי היא תהליך הכולל שליחת בקשת תקשורת ממחשב אחד לשרת עם מפתח אימות הנמצא בכותרת הודעת. למה כל הכאב? תלוי על מה אתם רוצים לעשות לכמה תכנון רשת שלך. עם זאת, אנו נעבוד על פיתוח יישום בלוק קוד לשליחה של HTTP בקשה עם אימות בסיסי באמצעות שפת סי.

## איך ל: 
כדי לשלוח בקשת HTTP עם אימות בסיסי בשפת סי, אתם צריכים לעקוב אחר שלושה שלבים פשוטים:

1. קבעו את שם המשתמש והסיסמה בפונקציה `curl_easy_setopt()` כך: ```C
curl_easy_setopt(curl, CURLOPT_USERNAME, "username");
curl_easy_setopt(curl, CURLOPT_PASSWORD, "password");
```
2. הוסיפו את שדה האימות בהאריך של `Authorization` בעזרת הפונקציה `curl_easy_setopt()` כך: ```C
curl_easy_setopt(curl, CURLOPT_HTTPHEADER, "Authorization: Basic");
```
3. שלחו את הבקשה באמצעות הפונקציה `curl_easy_perform()` כך: ```C
curl_easy_perform(curl);
```

השפה C מאפשרת גם לכם לקבוע את שדות האימות לבד במקום להשתמש בפונקציות `curl_easy_setopt()` על ידי הגדרת משתנים אלה ישירות בקוד.

## יצירת קשר עומקת: 
בעבר, האימות הבסיסי היה שיטה נפוצה להגנה על תקשורת בין שרת ללקוחות. בימינו, ישנן טכנולוגיות יותר מתקדמות המאפשרות הגנה טובה יותר כמו כונן SSL וכתבנית TLS. אופציית אימות בסיסי מוצגת בעיקר לצרכי מוסדיים.

עם זאת, אם יש לכם צורך מיוחד לשלוח בקשת HTTP עם אימות בסיסי, אתם עדיין יכולים לכתוב קוד יעיל ויעיל על ידי השתמשות בספריית libcurl הנפוצה ותיעוד corpsec נכון.

ישנן אפשרויות נוספות לאימות כגון Digest או NTLM, אך באמצעות האימות בסיסי ניתן לספק יכולת נוספת לשליחת בקשות באמצעות קוד סי.

## ראו גם: 
למידע נוסף על שליחת בקשות HTTP עם אימות בסיסי בשפת C, כדאי לקרוא את המסמכים הבאים:

- מדריך השימוש בממשק CURL של libcurl: https://curl.se/libcurl/c/CURLOPT_USERPWD.html
- מאמר באתר Medium על שליחת בקשות HTTP עם אימות בסיסי בשפת C: https://itnext.io/building-your-own-restful-api-with-basic-authentication-using-libcurl-in-c-2d5d6a14a65d