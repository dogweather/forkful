---
title:                "שימוש בביטויים רגולריים"
html_title:           "Bash: שימוש בביטויים רגולריים"
simple_title:         "שימוש בביטויים רגולריים"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## מה ולמה?
משתמשים בביטויים רגולריים (Regular Expressions) כדי לחפש טקסט, לאתר תבניות או להחליף מחרוזות במחרוזות אחרות. תוכניתנים משתמשים בהם מכיוון שהם כלי חזק לניתוח טקסטים, אוטומציה ובדיקות.

## איך לעשות:
חיפוש תווים פשוט במחרוזת:
```PowerShell
$text = "היי, איך הולך?"
if ($text -match "היי") {
  "מצאתי את התווים!"
} else {
  "לא מצאתי כלום."
}
```
לדוגמה זו תהיה הפלט: `מצאתי את התווים!`

חיפוש עם ביטוי רגולרי:

```PowerShell
$text = "שלח לדואר@domein.com את המייל"
$pattern = "\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z]{2,6}\b"
if ($text -match $pattern) {
  "נמצא כתובת מייל: $matches[0]"
} else {
  "לא מצאתי כתובת מייל"
}
```
פלט: `נמצא כתובת מייל: דואר@domein.com`

החלפת מחרוזת באמצעות ביטוי רגולרי:

```PowerShell
$text = "יש לי 100 דולר"
$pattern = "\d+"
$replacement = '300'
$newText = $text -replace $pattern, $replacement
$newText
```
פלט: `יש לי 300 דולר`

## צלילה עמוקה
ביטויים רגולריים החלו להתפתח בשנות ה-50 ומאז הם התמקדסו רבות. ישנם אלטרנטיבות כמו חיפוש טקסט ידני או שימוש בפונקציות מובנות לחיפוש מחרוזות אבל הם לא גמישים כמו ביטויים רגולריים. ב-PowerShell, ביטויים רגולריים ממומשים דרך ה .NET Framework, שלמעשה מאפשרת שימוש בהם גם בשפות תכנות אחרות.

## גם כדאי לראות
- [Learn Regex](https://regexr.com/) - מדריך אינטראקטיבי ואתר לבדיקת ביטויים רגולריים.
- [Regular-Expressions.info](https://www.regular-expressions.info/) - משאב המסביר בפירוט רב על ביטויים רגולריים.