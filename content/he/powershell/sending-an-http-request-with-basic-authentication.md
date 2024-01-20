---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "C: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP עם אימות בסיסי היא פרוסדורה שבה מנותבת בקשת HTTP אל שרת, אם מידע שמאמת את זהותו של השולח. מתכנתים עושים את זה כדי לאפשר אימות מהיר וקל לשימוש בסיסי.

## איך לעשות:
המשנה הבאה מראה לך איך לשלוח בקשת HTTP ב-PowerShell עם אימות בסיסי:
```PowerShell
# הגדרת שם משתמש וסיסמה
$userName = 'YourUsername'
$password = 'YourPassword'

# יצירת אובייקט הסיסמה
$secStringPassword = ConvertTo-SecureString -String $password -AsPlainText -Force

# יצירת אובייקט האימות
$credObject = New-Object -TypeName System.Management.Automation.PSCredential -ArgumentList $userName, $secStringPassword

# שליחת בקשת GET לשרת
Invoke-WebRequest -Uri 'http://yourserver.com' -Method GET -Credential $credObject
```
פלט הדוגמא הנ"ל הוא התשובה מהשרת - XML, JSON, HTML, טקסט, וכו'.

## צפיה עמוקה
שליחת בקשת HTTP עם אימות בסיסי היא מתודה ישנה שמורשת מתקני URI מהאינטרנט הקדום. חלופות יכולות לכלול אימות OAuth או JWT. כאשר PowerShell שולח בקשה, הוא מפענח את פרטי האימות למחרוזת Base64 ומוסיף אותה ככותרת של הבקשה.

## ראה גם:
* [התקנים HTTP](https://www.ietf.org/rfc/rfc2616.txt)
* [המסמך המקורי של Invoke-WebRequest](https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7)
* [מדריכי PowerShell שונים](https://www.powershelltutorial.net/)