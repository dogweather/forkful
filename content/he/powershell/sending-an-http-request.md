---
title:                "שליחת בקשת http"
html_title:           "Bash: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## מה זה ולמה? 

שליחת בקשת HTTP היא ביצוע בקשה לשרת על ידי שימוש בפרוטוקול HTTP. מתכנתים מבצעים את זה כדי לגשת למידע או לשלוט במאפיינים של השרת.

## איך ל:
```PowerShell
# התקנה של מודול ה-HTTP:
Install-PackageProvider Nuget –Force
Install-Module -Name Powershell-InvokeWebRequestWrapper

# שליחת בקשת GET
$response = Invoke-WebRequest -Uri "http://your-url.com"
# הדפסת התגובה
$response.Content
```

## צלילה עמוקה

האם ידעת? ה-Powershell Invoke-WebRequest נוצר ב2012 כחלק מ-Powershell v3. חלופות ל-PowerShell כוללות cURL או אינטגרציה של שפות תכנות אחרות כמו Python או Node.js.

בפרט, שימו לב שהשיטה Invoke-WebRequest של PowerShell משתמשת במתודת GET כברירת מחדל. אם ברצונך לשנות את השיטה, תוכל לעשות זאת באמצעות הפרמטר -Method.

## ראה גם

* [למידת PowerShell](https://docs.microsoft.com/he-il/powershell/scripting/overview?view=powershell-7.1): תיעוד הרשמי של Microsoft של שפת תכנות PowerShell.
* [מסמך ה-API של פרוטוקול HTTP](https://tools.ietf.org/html/rfc2616): מסמך ה-RFC המוגדר לפרוטוקול ה-HTTP.
* [ספר ב-Powershell Scripting](https://www.amazon.com/Windows-PowerShell-Scripting-Guide-Ed/dp/0735675112): ספר אשר מעמיק יותר לגבי שפת תכנות Powershell.