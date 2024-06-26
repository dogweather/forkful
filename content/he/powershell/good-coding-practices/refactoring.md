---
date: 2024-01-26 03:40:50.253829-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-PowerShell\
  \ \u05D0\u05D9\u05DF \u05DB\u05DC\u05D9 \u05E8\u05D9\u05E4\u05E7\u05D8\u05D5\u05E8\
  \u05D9\u05E0\u05D2 \u05DE\u05D5\u05D1\u05E0\u05D4, \u05D0\u05DA \u05E0\u05D9\u05EA\
  \u05DF \u05E2\u05D3\u05D9\u05D9\u05DF \u05DC\u05E0\u05E7\u05D5\u05EA \u05D0\u05EA\
  \ \u05D4\u05E7\u05D5\u05D3 \u05E9\u05DC\u05DB\u05DD \u05DC\u05E6\u05D5\u05E8\u05DA\
  \ \u05E7\u05E8\u05D9\u05D0\u05D5\u05EA \u05D5\u05D1\u05D9\u05E6\u05D5\u05E2\u05D9\
  \u05DD. \u05D1\u05D7\u05E9\u05D1\u05D5\u05DF \u05E0\u05EA\u05DF \u05E9\u05DC \u05E4\
  \u05D5\u05E0\u05E7\u05E6\u05D9\u05D4 \u05E9\u05E2\u05D5\u05E9\u05D4 \u05D9\u05D5\
  \u05EA\u05E8 \u05DE\u05D3\u05D9 \u05D5\u05D0\u05D9\u05DA \u05D0\u05E4\u05E9\u05E8\
  \u2026"
lastmod: '2024-03-13T22:44:39.712817-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-PowerShell \u05D0\u05D9\u05DF \u05DB\u05DC\u05D9 \u05E8\u05D9\u05E4\
  \u05E7\u05D8\u05D5\u05E8\u05D9\u05E0\u05D2 \u05DE\u05D5\u05D1\u05E0\u05D4, \u05D0\
  \u05DA \u05E0\u05D9\u05EA\u05DF \u05E2\u05D3\u05D9\u05D9\u05DF \u05DC\u05E0\u05E7\
  \u05D5\u05EA \u05D0\u05EA \u05D4\u05E7\u05D5\u05D3 \u05E9\u05DC\u05DB\u05DD \u05DC\
  \u05E6\u05D5\u05E8\u05DA \u05E7\u05E8\u05D9\u05D0\u05D5\u05EA \u05D5\u05D1\u05D9\
  \u05E6\u05D5\u05E2\u05D9\u05DD."
title: "\u05E8\u05E4\u05E7\u05D8\u05D5\u05E8\u05D9\u05E0\u05D2"
weight: 19
---

## איך לעשות:
ב-PowerShell אין כלי ריפקטורינג מובנה, אך ניתן עדיין לנקות את הקוד שלכם לצורך קריאות וביצועים. בחשבון נתן של פונקציה שעושה יותר מדי ואיך אפשר לבצע בה ריפקטורינג למען הבהרה:
```PowerShell
function Get-InventoryData {
    # פונקציה מקורית המשלבת איסוף נתונים ועיצוב
    $data = Get-Content -Path 'C:\inventory-list.txt'
    $inventoryData = $data | ForEach-Object {
        $fields = $_ -split ','
        [PSCustomObject]@{
            ItemID = $fields[0]
            Name   = $fields[1]
            Count  = $fields[2]
            Price  = $fields[3]
        }
    }
    $inventoryData | Format-Table -AutoSize
}

# נחלק לפונקציות נפרדות
function Import-InventoryData {
    param($Path)
    Get-Content -Path $Path | ForEach-Object {
        $fields = $_ -split ','
        [PSCustomObject]@{
            ItemID = $fields[0]
            Name   = $fields[1]
            Count  = $fields[2]
            Price  = $fields[3]
        }
    }
}

function Format-InventoryData {
    param($Data)
    $Data | Format-Table -AutoSize
}

# שימוש
$inventory = Import-InventoryData -Path 'C:\inventory-list.txt'
Format-InventoryData -Data $inventory
```

דוגמא לפלט:

```
ItemID Name            Count Price
------ ----            ----- -----
1001   Widget Type A   50    9.99
1002   Gadget Type B   20    14.99
```

## צלילה עמוקה
הריפקטורינג בתכנות נזכר לראשונה עוד בימים הראשונים של פיתוח התוכנה, אך הופורמל כשיטה בשנות ה-90. הספר של מרטין פאולר, "Refactoring: Improving the Design of Existing Code", הוא אחד מהיצירות המכוננות בנושא, המדגיש את חשיבות הריפקטורינג בהשגת קוד נקי.

למרות שב-PowerShell אין כלים ספציפיים לריפקטורינג כמו בסביבות פיתוח אינטגרטיביות (IDEs) לשפות אחרות (כמו Eclipse או Visual Studio), ניתן עדיין לתרגל עקרונות ריפקטורינג טובים באופן ידני. הדבר החשוב לזכור הוא שריפקטורינג אינו רק על שינוי קוד למען השינוי, אלא על ביצוע שינויים מודעים ושומרי התנהגות שמשפרים את מבנה ועיצוב הקוד.

חלופות לריפקטורינג ידני ב-PowerShell כוללות שימוש ב-IDEs התומכים בשפה, כמו Visual Studio Code עם התוסף של PowerShell, המציעים תכונות כמו עיצוב קוד ויכולות ריפקטורינג בסיסיות. לשינויים ריפקטורינג משמעותיים יותר, עשוי להיות שימושי להשתמש בבדיקות Pester כדי לוודא שהשינויים לא משנים את הפונקציונליות.

בנוסף, יישום של ריפקטורינג יכול לכלול שינויים מערכתיים נוספים כמו מודולריזציה, שבה קוד מחולק למודולים או פונקציות ניתנים לשימוש מחדש, מה שמשפר את היעדר חזרה על עצמו (DRY - Don't Repeat Yourself). טכניקות ריפקטורינג נפוצות נוספות כוללות שינוי שמות למען הבהרה, הסרת קוד כפול, והפחתת מורכבות של לוגיקה תנאית.

## ראו גם
להרחבה, הנה כמה משאבים:

- ספר הריפקטורינג של מרטין פאולר: [_Refactoring: Improving the Design of Existing Code_](https://martinfowler.com/books/refactoring.html)
- בדיקת קוד מרופקטור עם Pester: [מסגרת הבדיקות Pester](https://pester.dev/)
- מתכונות השימוש הטובות ביותר ב-PowerShell: [המדריך למתכונות השימוש הטובות ביותר ולסגנון של PowerShell](https://poshcode.gitbooks.io/powershell-practice-and-style/)
