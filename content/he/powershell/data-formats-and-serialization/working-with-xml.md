---
date: 2024-01-26 04:35:08.453018-07:00
description: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML \u05DB\u05D5\u05DC\u05DC\
  \u05EA \u05E2\u05D9\u05D1\u05D5\u05D3 \u05D5\u05D2\u05D9\u05E9\u05D4 \u05DC\u05E0\
  \u05EA\u05D5\u05E0\u05D9\u05DD \u05DE\u05D5\u05D1\u05E0\u05D9\u05DD \u05D1\u05E9\
  \u05E4\u05EA \u05D4-eXtensible Markup Language. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\
  \u05DD \u05E2\u05D5\u05D1\u05D3\u05D9\u05DD \u05E2\u05DD XML \u05E2\u05DC \u05DE\
  \u05E0\u05EA \u05DC\u05D0\u05E4\u05E9\u05E8 \u05D0\u05D9\u05E0\u05D8\u05E8\u05D5\
  \u05E4\u05E8\u05D1\u05D9\u05DC\u05D9\u05D5\u05EA \u05E2\u05DD \u05DE\u05E2\u05E8\
  \u05DB\u05D5\u05EA \u05D0\u05D7\u05E8\u05D5\u05EA \u05D0\u05D5\u2026"
lastmod: '2024-02-25T18:49:37.978057-07:00'
model: gpt-4-0125-preview
summary: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML \u05DB\u05D5\u05DC\u05DC\
  \u05EA \u05E2\u05D9\u05D1\u05D5\u05D3 \u05D5\u05D2\u05D9\u05E9\u05D4 \u05DC\u05E0\
  \u05EA\u05D5\u05E0\u05D9\u05DD \u05DE\u05D5\u05D1\u05E0\u05D9\u05DD \u05D1\u05E9\
  \u05E4\u05EA \u05D4-eXtensible Markup Language. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\
  \u05DD \u05E2\u05D5\u05D1\u05D3\u05D9\u05DD \u05E2\u05DD XML \u05E2\u05DC \u05DE\
  \u05E0\u05EA \u05DC\u05D0\u05E4\u05E9\u05E8 \u05D0\u05D9\u05E0\u05D8\u05E8\u05D5\
  \u05E4\u05E8\u05D1\u05D9\u05DC\u05D9\u05D5\u05EA \u05E2\u05DD \u05DE\u05E2\u05E8\
  \u05DB\u05D5\u05EA \u05D0\u05D7\u05E8\u05D5\u05EA \u05D0\u05D5\u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML"
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
