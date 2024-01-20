---
title:                "הורדת דף אינטרנט"
html_title:           "C++: הורדת דף אינטרנט"
simple_title:         "הורדת דף אינטרנט"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## מה ולמה?
הורדת דף אינטרנט היא תהליך של שליפה של קוד HTML של דף אינטרנט. מתכנתים עושים את זה כדי לגשת ולבחון מידע ממקורות בחינם באינטרנט, או לטפל במשימות אוטומטיות.

## איך לעשות:
הנה דוגמה של איך להוריד דף אינטרנט באמצעות PowerShell:

```PowerShell
# מאפשר לך להחזיק את האתר במשתנה.
$Uri = 'https://www.google.com'

# השתמש בפקודת Invoke-WebRequest כדי להוריד את הדף.
$Page = Invoke-WebRequest -Uri $Uri

# הדפס את התוכן של הדף.
$Page.Content
```

שים לב לפלט שנוצר:

```PowerShell
<!doctype html><html itemscope="" itemtype="http://schema.org/WebPage" lang="he"><head><meta content="text/html; charset=UTF-8" http-equiv="Content-Type"><meta content="..."
```

## קנה מידע
הורדת דף אינטרנט היא טכניקה שמשמשת מהימים הראשונים של האינטרנט, רק שהכלים המשמשים לזה השתנו. PowerShell, בתור שפת תכנות של מיקרוסופט, מציע פונקציונליות מובנית לשם זה דרך פקודת `Invoke-WebRequest`.

חלופות ל-PowerShell להורדת דף אינטרנט כוללות למשל curl ו wget, שהן אף משמשות בסביבות Unix-המבוססות.

שים לב שבדוגמה שלנו, אנו משתמשים בערך שמוחזר מ- `Invoke-WebRequest` כדי להציג את תוכן הדף שהורד. זה מעניק לנו גישה לקוד.HTML של הדף, אך המידע יהיה מבולגן אם הדף מכיל JavaScript או CSS.

## ראו גם: 
- [דוקומנטציה רשמית של מיקרוסופט על `Invoke-WebRequest`](https://docs.microsoft.com/en-us/powershell/module/Microsoft.PowerShell.Utility/Invoke-WebRequest)

- [הסבר מקיף על מודל האובייקטים של PowerShell](https://www.red-gate.com/simple-talk/sysadmin/powershell/powershell-data-basics-file-based-data/)