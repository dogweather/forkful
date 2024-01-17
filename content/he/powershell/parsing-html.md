---
title:                "פיענוח HTML"
html_title:           "PowerShell: פיענוח HTML"
simple_title:         "פיענוח HTML"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/parsing-html.md"
---

{{< edit_this_page >}}

# מה זה ולמה?

Parsing HTML הוא תהליך של קריאת קוד HTML והמרתו לנתונים ישירות. תהליך זה חשוב לתוכניות המשתמשות בנתונים מאתרי אינטרנט, כמו למשל חיפושי אינטרנט.

## איך לבצע?

```PowerShell
# התקנת המודול הדרוש
Install-Module HtmlAgilityPack

# יצירת אובייקט HTML וטעינתו מאתר אינטרנט
$html = Invoke-WebRequest -URI "www.example.com"

# קריאת תוכן האתר ושמירתו במשתנה $content
$content = $html.Content

# שימוש בפונקציה המתאימה כדי למצוא את הנתונים הרצויים
$links = $content | Select-String -Pattern '<a href="(.*?)">' -AllMatches
```

כמו בדוגמה הנ"ל, קוד PowerShell ניתן לשימוש בכדי לטעון את קוד HTML ולמצוא את הנתונים הרצויים באמצעות שימוש בפונקציות כגון "Select-String" ו-"Regular Expressions". 

## חפירה מעמיקה

החפירה בקוד HTML היא חלק חשוב מתהליך פיתוח תוכנות. כיום, ישנם מספר מודולים וספריות פופולריים שניתן להשתמש בהם כדי לקרוא ולנתח קוד HTML בכל שפה שתבחרו. חשוב לדעת שישנם גם אפשרויות אחרות כמו לדוגמה גישת API לאתרים יחד עם גדול מופעי האתר, על מנת לקבל גישה מהירה יותר לנתונים.

המודול המצוין בדוגמה הנ"ל הוא האינטגרציה של HtmlAgilityPack בפקודות PowerShell בכדי לספק ממשק נוח למציאת, התייחסות ועבודה עם קוד HTML.

## למידע נוסף

למידע נוסף על parsing HTML ב-PowerShell מומלץ לקרוא את המדריך המצוין באתר הרשמי של PowerShell ואת חומרי הלימוד השונים ברשת.

שימושים נוספים של קוד PowerShell כוללים יכולת ליצור סקריפטים מתוחכמים יותר ולתפעול על מכשירים מרוחקים. למתחילים, כדאי להתחיל עם הקורס המקוון הנ"ל - https://www.youtube.com/watch?v=KA9pzla8JWE.