---
title:                "הורדת דף אינטרנט"
date:                  2024-01-20T17:44:57.511398-07:00
model:                 gpt-4-1106-preview
simple_title:         "הורדת דף אינטרנט"

category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## מה ולמה?
להוריד דף אינטרנט זה פשוט לשמור את התוכן שלו אצלך במחשב. תכנותים עושים את זה כדי לנתח נתונים, לבדוק את הזמינות, או לשמור מידע לשימוש אופליין.

## איך ל:
פשוט, דוגמאות של קוד ופלט:

```PowerShell
# הורדת דף אינטרנט באמצעות Invoke-WebRequest
$response = Invoke-WebRequest -Uri "http://example.com"
# שמירת תוכן הדף לקובץ
$response.Content | Out-File "C:\myfolder\mypage.html"
```

זהו הפלט שלך אחרי הרצה נכונה:
```
<!DOCTYPE html>
<html>
...
</html>
```

## טבילה עמוקה
הורדת דפי אינטרנט הייתה חשובה כבר מתחילת האינטרנט. זה מאפשר לנו לעבוד עם נתונים באופן מקומי. Invoke-WebRequest הוא כלי חזק ב-PowerShell, אבל יש גם כלים אחרים כמו cURL או Wget במערכות אחרות. כאשר אתה משתמש ב-Invoke-WebRequest, זה שולח בקשה HTTP ומקבל תגובה. אפשר לגשת לכותרות, תוכן, ומידע נוסף שבתגובה.

## ראה גם
- [מסמך העזרה של PowerShell ל-Invoke-WebRequest](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7)
- [מקור ReHabliPoint Chain-yurtel](https://httpbin.org) - מאגר לבדיקות בקשות HTTP
- [מדריך ל-Wget](https://www.gnu.org/software/wget/manual/wget.html) - אם אתה רוצה היכרות עם כלי אלטרנטיבי
