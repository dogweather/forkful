---
title:                "הורדת עמוד אינטרנט"
html_title:           "PowerShell: הורדת עמוד אינטרנט"
simple_title:         "הורדת עמוד אינטרנט"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## מה זה ולמה?
הורדת עמוד אינטרנט היא פעולה שבה מתכנתים משתמשים כדי לקבל את התוכן של עמוד אינטרנט בצורה אוטומטית. הפעולה הזו נפוצה מאוד בפיתוח אפליקציות המשתמשות בתוכן מהאינטרנט כמו ממשקי API או בוטים.

## איך לעשות:
כדי להוריד עמוד אינטרנט באמצעות פוורשל, ניתן להשתמש בפקודה המובנית Invoke-WebRequest. כדי לעשות זאת, מספקים את הכתובת של העמוד כמערך של ערכים לפקודה ואז משתמשים בפקודה 'Content | Out-File' כדי להציג את התוצאות בקובץ טקסט חדש. להלן דוגמה של פקודה זו:
```PowerShell
Invoke-WebRequest -Uri "https://www.example.com" | Select-Object -ExpandProperty Content | Out-File webpage.txt
```

## חקירה מעמיקה:
על הפקודה Invoke-WebRequest היא חלק מהמודול "Microsoft.PowerShell.Utility" ומתאימה לגרסת 4.0 של פוורשל. ישנם גם תוספים נוספים להורדת עמודים אינטרנט כמו Invoke-WebRequset ו- Invoke-RestMethod. כמו כן, ניתן להשתמש בכלי חיצוני כמו Curl או Wget.

## לצפות בגם:
למידע נוסף על הפקודה Invoke-WebRequest ניתן לעיין במדריכים הבאים:
- מסמכי המטה Microsoft [Invoke-WebRequest](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7.1)
- המאמר הבא מאת אבי ווד, [Invoke-WebRequest - Web Scraping Part I](https://avlidienbrunn.com/2018/04/invoke-webrequest-web-scraping-part-i/)
- דוגמאות להורדת עמודים אינטרנט עם PowerShell [here](https://www.codetwo.com/admins-blog/how-to-download-a-web-page-using-powershell/)