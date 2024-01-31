---
title:                "רפקטורינג"
date:                  2024-01-26T03:40:50.253829-07:00
model:                 gpt-4-0125-preview
simple_title:         "רפקטורינג"

category:             "PowerShell"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/refactoring.md"
---

{{< edit_this_page >}}

## מה ולמה?
ריפקטורינג הוא תהליך של שינוי מבנה של קוד מחשב קיים מבלי לשנות את התנהגותו החיצונית, במטרה לשפר תכונות לא פונקציונליות של התוכנה. מתכנתים מבצעים ריפקטורינג לקוד כדי להפוך אותו לנקי יותר, יעיל יותר וקל יותר להבנה, מה שמקל על תחזוקה קלה יותר ושדרוגים עתידיים.

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
