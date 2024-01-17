---
title:                "התחלת פרויקט חדש"
html_title:           "PowerShell: התחלת פרויקט חדש"
simple_title:         "התחלת פרויקט חדש"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/starting-a-new-project.md"
---

{{< edit_this_page >}}

# מה ולמה?

התחלת פרויקט חדש היא פעולה בדיוק כפי שמשמעו שמה - התחלת עבודה על פרויקט חדש. כמו בכל עבודה חדשה, זה מתחיל עם רעיון ואתחול עליו בפועל. תכנתנים עושים את זה כדי ליצור משהו חדש ומיוחד, כדי לפתוח עסק חדש או לפתח יכולת חדשה במערכת קיימת.

# איך לעשות זאת:

 ```PowerShell
 # הגדרת משתנה עם כותרת לפרויקט חדש
 $projectName = "פרויקט חדש"
 
 # הפקודה הבאה תצור תיקייה חדשה עם השם שלנו
 New-Item -ItemType Directory -Name $projectName
 
 # הדפסת הודעה אישור
 Write-Host "נוצרה תיקייה חדשה בשם $projectName"
 
 # הפקודה הבאה תצור קובץ חדש עם השם שלנו
 New-Item -ItemType File -Name "main.ps1"
 
 # בדיקה שהקובץ נוצר בהצלחה
 if (Test-Path ".\main.ps1") {
     Write-Host "הקובץ main.ps1 נוצר בהצלחה"
 }
 
 
 ```
 
 פלט:
 
נוצרה תיקייה חדשה בשם פרויקט חדש<br>
הקובץ main.ps1 נוצר בהצלחה

# כייף יותר:

לפני שנתחיל לכתוב קוד, ניתן להשתמש בתבנית קיימת כדי להפוך את התחלת הפרויקט לקלה ומהירה יותר. קיימות כמה תבניות מוכנות לשימוש, כגון תבנית עם תפריט ותבנית ריקה לבחירתך. כמובן שניתן גם ליצור תבניות משלך על מנת להתאים אותן לצרכים ספציפיים שלך.

# חקירה מעמיקה:

כדי להתחיל פרויקט חדש בפווורשל, ניתן להשתמש בפקודת "New-Item" כפי שראינו בדוגמאות. אם תרצו לתת שם לתיקיית הפרויקט, ניתן להשתמש בפקודת "Rename-Item" ולשנות את השם הקיים. כמו כן, ניתן להשתמש גם בתחילת כל פרויקט עם כמה קבצים ולספק שם לכל אחד מאלה באמצעות "Rename-Item" או יכול ליצור תיקיות נוספות עם "New-Item".

# ראו גם:

למידע נוסף על פקודות בפווורשל, ניתן לעיין במקורות הבאים:

1. תיעוד רשמי של מיקרוסופט - https://docs.microsoft.com/en-us/powershell/scripting/overview?view=powershell-7.1
2. מאמרים ומדריכים נוספים בבלוגים ואתרים כמו הבלוג של PowerShellMagazine - https://www.powershellmagazine.com/
3. פורומים וקהילות מקצועיות לשאלות ותשובות - https://stackoverflow.com/ ו- https://powershell.org/