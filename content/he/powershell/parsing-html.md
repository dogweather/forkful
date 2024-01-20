---
title:                "ניתוח HTML"
html_title:           "Arduino: ניתוח HTML"
simple_title:         "ניתוח HTML"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/parsing-html.md"
---

{{< edit_this_page >}}

## מה ולמה?

פענוח HTML הוא פעולת ניתוח של קוד HTML לצורך שימוש או מטרה מסויימת. התכנתים עושים את זה כדי לקרוא, לשפר או לאחזר מידע מאתרי אינטרנט.

## איך:

```PowerShell
# התקנה של Invoke-WebRequest
Install-PackageProvider -Name NuGet -MinimumVersion 2.8.5.201 -Force
Install-Module -Name Invoke-WebRequest -Force

# פענוח HTML עם PowerShell
$URL = 'https://www.example.com'
$HTML = Invoke-WebRequest -Uri $URL
$ParsedHTML = $HTML.ParsedHtml

$ParsedHTML.getElementsByTagName('tag_name') | foreach {$_.innerText}
```
פלט דוגמה:
```PowerShell
Element text 1
Element text 2
Element text 3
```

## הצוללת המעמיקה:

### ההקשר ההיסטורי

PowerShell הושקה ב-2006 כאמצעי ממוחשב לניהול ואוטומציה של משימות בעזרת סריפטים.

### חלופות

עוד שפות תכנות שיכולות לפענח HTML הן Python (עם BeautifulSoup) וJavaScript (עם Node.js).

### פרטים בנוגע ליישום

הפקודה `Invoke-WebRequest` משתמשת ב-object מסוג HTMLDocument של MSHTML - COM object של Microsoft המספק את אפשרות הפענוח.

## ראה גם:

- [Invoke-WebRequest official documentation](https://docs.microsoft.com/he-il/powershell/scripting/learn/deep-dives/everything-about-html-parsing?view=powershell-7.1)
- [HTML parsing in Python](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- [HTML parsing in Node.js](https://cheerio.js.org/)