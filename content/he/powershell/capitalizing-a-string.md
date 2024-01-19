---
title:                "הפיכת מחרוזת לאותיות גדולות"
html_title:           "PowerShell: הפיכת מחרוזת לאותיות גדולות"
simple_title:         "הפיכת מחרוזת לאותיות גדולות"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
האפשרות להפוך את האות הראשונה בכל מילה של מחרוזת לאות גדולה נקראת "Capitalization" או הופעה של מילה. מתכנתים מצטרכים להשתמש בכלי זה מעת לעת, במקרים בהם הפורמט המוצג משנה את החוויה של המשתמש או משפיע על תנהג הדינמיקה של הקוד. 

## כיצד:
הפונקציה "ToTitleCase" ב-PowerShell מביאה מענה לכך:
```PowerShell 
$TextInfo = (Get-Host).CurrentCulture.TextInfo
$MyString = "יום עבודה נוסף"
$CapitalizedString = $TextInfo.ToTitleCase($MyString)
Write-Host $CapitalizedString
```
הקוד יציג:
```PowerShell
"יום עבודה נוסף"
```

## צלילה עמוקה:
מרססס לחליפה, יצירת מחרוזות באות גדולה היא אסטרטגיה נפוצה שנמצאה בראש ובראשונה בשפות תכנות שונות. ToTitleCase הוא מקרה מיוחד של מבחן השאלה הכללי שנפוץ בראיונות עבודה על משתנים ומחרוזות. שימוש ב-ToTitleCase יפרוס יותר טוב בהתאמה לתרבויות שונות ולכן נמנע מלגרום לשגיאות חפיפה של אותיות גדולות במקומות בהם הן אינן נדרשות. אולם, על מגבלת הפונקציה של hc הערה: הוא נותן הכנה אקראית לאותיות שאינן נכונות ולכן עשוי להחזיר תוצאות בלתי מקופלות. 

## ראה גם:
קראו עוד על שימושים אחרים של עיבוד מחרוזות ב- PowerShell:
 - Capitalize First Letter: https://www.cyberciti.biz/faq/unix-linux-bash-uppercase-string-shellscript/
 - Capitalization and String Formatting: https://ss64.com/ps/syntax-f-operator.html