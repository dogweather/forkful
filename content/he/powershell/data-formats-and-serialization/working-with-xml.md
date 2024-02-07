---
title:                "עבודה עם XML"
date:                  2024-01-26T04:35:08.453018-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/working-with-xml.md"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם XML כוללת עיבוד וגישה לנתונים מובנים בשפת  ה-eXtensible Markup Language. מתכנתים עובדים עם XML על מנת לאפשר אינטרופרביליות עם מערכות אחרות או לקרוא ולכתוב קבצי תצורה, הזנות נתונים, ומסמכים מובנים אחרים הנפוצים בשירותי רשת.

## איך ל:
```PowerShell
# טוען קובץ XML לתוך משתנה
[xml]$xmlContent = Get-Content 'path\to\your\file.xml'

# גישה לצמתי XML
$books = $xmlContent.catalog.book
foreach ($book in $books) {
  Write-Output "כותרת: $($book.title)"
}

# יצירת אלמנט XML חדש
$newBook = $xmlContent.CreateElement("book")
$newBook.SetAttribute("id", "bk999")
$xmlContent.DocumentElement.AppendChild($newBook)

# שמירת ה-XML בחזרה לקובץ
$xmlContent.Save('path\to\your\updated\file.xml')
```
פלט לדוגמה:
```
כותרת: תכנות PowerShell
כותרת: יסודות XML
```

## צלילה עמוקה
XML, או eXtensible Markup Language, קיימת מאז שנות ה-90 המאוחרות ונותרת פורמט נפוץ לנתונים מובנים. PowerShell מפשטת את העבודה עם XML בהשוואה לשיטות ניתוח קלאסיות; היא מטילה את XML לעצמים באופן ישיר, מה שמאפשר לך להתמודד עם אלמנטים דרך התחביר המוכר של נוטציית הנקודה.

חלופות ל-XML כוללות JSON, YAML, או פורמטים מותאמים אישית של נתונים. JSON, לדוגמא, הפך לפופולרי בזכות משקלו הקל והקלות של שימוש עם טכנולוגיות רשת. עם זאת, התכונות המורחבות של XML כמו מרחבי שמות, סכמות, ועיבוד XSLT לעיתים קרובות הופכות אותו למתאים יותר למסמכים מורכבים או לתקנות תעשייתיות.

PowerShell משתמשת ביכולות ה-XML של .NET Framework לטיפול ב-XML. זה אומר שלא מדובר רק בפעולות קריאה-כתיבה פשוטות; אתה יכול גם לעבוד עם סכמות XML לאימות, להשתמש ב-XPath לשאילתות, ולהעסיק טרנספורמציות XSLT, הכל דרך PowerShell.

## ראה גם
- [מדריך XML של W3Schools](https://www.w3schools.com/xml/)
- [XML לעומת JSON](https://www.json.org/json-en.html)
