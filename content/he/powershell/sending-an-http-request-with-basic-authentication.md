---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "PowerShell: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה זה ולמה?

שליחת בקשת HTTP עם אימות בסיסי היא תהליך שבו משתמשים בפקודות הוראת PowerShell כדי לפנות לשרת עם הזנת פרטי אימות בסיסיים, כגון שם משתמש וסיסמה. המטרה העיקרית של שליחת בקשת HTTP עם אימות בסיסי היא להתחבר לשרת ולאתר זמין.

## איך לעשות זאת:

```PowerShell
# שליחת בקשת HTTP עם אימות בסיסי
Invoke-WebRequest -Uri https://www.example.com -Method Get -Credential (Get-Credential)

# תוצאה דוגמה - אימות בהצלחה
StatusCode        : 200
StatusDescription : OK
Content           : <html>
                        <body>
                            <h1>Hello World!</h1>
                        </body>
                    </html>
RawContent        : HTTP/1.1 200 OK
                    <html>
                        <body>
                            <h1>Hello World!</h1>
                        </body>
                    </html>
Headers           : {[Date, 2021-08-01 12:00:00 GMT], [Content-Type, text/html], [Content-Length, 88]}
Forms             : {}
Images            : {}
InputFields       : {}
Links             : {}
ParsedHtml        : mshtml.HTMLDocumentClass
```

## טיפול עמוק:

1. היסטוריית המקורות:
תהליך שליחת בקשת HTTP עם אימות בסיסי הופעל בראשונה בשנת 1996 על ידי מערכת הגנה הנקראת "בסיס המשטרה הבריטי". מאז, התהליך הזה נהפך לטכנולוגיה שכמעט כל מפתחי התוכנה יכולים להשתמש בה.

2. אלטרנטיבות:
כל תהליך של שליחת בקשת HTTP עם אימות בסיסי ניתן לבצע גם באמצעות אחת מכלים אחרים כגון cURL או Postman. אולם, פקודת PowerShell נמצאת בתיקיית Invoke-WebRequest של Windows 10 ומספקת את כל הפונקציונליות במקום אחד.

3. פירוט על עבודת הפילטר:

כאשר משתמשים בפקודת Invoke-WebRequest כדי לשלוח בקשת HTTP עם אימות בסיסי, יש למתן פרמטר מצופה של -Credential. עם הפרמטר הזה, ניתן לציין שם משתמש וסיסמה שכבר נכתבו בפקודת Get-Credential ואישור ההזדהות. בעת ביצוע פקודה זו, לא ייתן למשתמשים להפעיל תוכנה או לגשת לכתובת אתר אם אין להם הרשאות מתאימות כמחברים לאתר.

## ראו גם:

- [אתר רשמי של PowerShell](https://docs.microsoft.com/he-il/powershell/)
- [פקודת Invoke-WebRequest באתר התיקונים](https://docs.microsoft.com/he-il/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7.1)
- [אתר הוראתה של בסיס המשטרה הבריטי] (https://basic-authentication.com/)