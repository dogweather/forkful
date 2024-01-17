---
title:                "שליחת בקשת http"
html_title:           "PowerShell: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/sending-an-http-request.md"
---

{{< edit_this_page >}}

# מה ולמה?
שליחת בקשת HTTP היא פעולה נפוצה בתכנות, שבה מתבצעת בקשה לאתר אינטרנט כדי לקבל מידע מסוים. מתכנתים מבצעים פעולה זו כדי לתקשר עם שרתים וליצור תקשורת בין האפליקציות שלהם לשירותים חיצוניים ברשת.

# כיצד לבצע?
תחת ```PowerShell ... ``` ניתן לראות כמה דוגמאות של שליחת בקשת HTTP תוך השתמשות בפקודות השונות שמציגות את הפעולה בפועל. למשתמש תופיע המידע המבוקש מהאתר המבוקש בתוך הפקודה.

```PowerShell
# דוגמא 1 
Invoke-WebRequest 'https://www.example.com' # שולח בקשה לאתר ומציג את התוצאה המלאה של האתר

# דוגמא 2
Invoke-WebRequest -Method GET -Uri 'https://www.example.com' # שולח בקשה מדורגת על פי הפעולה המבוקשת ומציג את התוצאה המלאה
```

# צלילה עמוקה
שליחת בקשת HTTP נעשית מאז שתחילת התכנות והיא נמצאת בשימוש רב בכלי תכנון ובתחומי האפליקציות השונות. פקודות אחרות כמו Invoke-RestMethod ו-Invoke-WebRequest גם הן נעשות באופן דמיוני לשליחת בקשת HTTP. חשוב לוודא שמתחברים לאתר באמצעות פרוטוקול חזק ומאובטח כמו HTTPS כדי להגן על המידע הנשלח והמקבל.

# ראה גם
למידע נוסף על שמות הפקודות ואפשרויות נוספות, בדוק את המדריך הרשמי של PowerShell על שליחת בקשת HTTP.
https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7

למידע נוסף על פקודת Invoke-RestMethod, יש לעיין בתיעוד הרשמי של Microsoft.
https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod?view=powershell-7

למידע מורחב על פעולת השליחה של הבקשה, אפשר לראות את מדריך הקוד הפתוח של Microsoft על Invoke-WebRequest.
https://github.com/PowerShell/PowerShell/blob/24ad6da1993d742d28eb19613ffde1c4dd861e41/src/Microsoft.PowerShell.Commands.Utility/commands/utility/InvokeWebRequest.cs