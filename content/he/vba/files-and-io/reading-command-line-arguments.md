---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:00:18.280580-07:00
description: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05D0\u05E8\u05D2\u05D5\u05DE\u05E0\u05D8\
  \u05D9\u05DD \u05E9\u05DC \u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\
  \u05D4 \u05D1-Visual Basic for Applications (VBA) \u05DB\u05D5\u05DC\u05DC\u05EA\
  \ \u05D2\u05D9\u05E9\u05D4 \u05DC\u05E4\u05E8\u05DE\u05D8\u05E8\u05D9\u05DD \u05E9\
  \u05D4\u05D5\u05E2\u05D1\u05E8\u05D5 \u05DC\u05EA\u05D5\u05DB\u05E0\u05D9\u05EA\
  \ \u05E9\u05DC\u05DA \u05D1\u05E2\u05EA \u05D4\u05D4\u05E4\u05E2\u05DC\u05D4. \u05D4\
  \u05D8\u05DB\u05E0\u05D9\u05E7\u05D4 \u05D4\u05D6\u05D5 \u05DE\u05E9\u05DE\u05E9\
  \u05EA \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD \u05E7\u05E8\u05D5\u05D1\u05D5\u05EA\
  \u2026"
lastmod: 2024-02-19 22:04:58.303734
model: gpt-4-0125-preview
summary: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05D0\u05E8\u05D2\u05D5\u05DE\u05E0\u05D8\
  \u05D9\u05DD \u05E9\u05DC \u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\
  \u05D4 \u05D1-Visual Basic for Applications (VBA) \u05DB\u05D5\u05DC\u05DC\u05EA\
  \ \u05D2\u05D9\u05E9\u05D4 \u05DC\u05E4\u05E8\u05DE\u05D8\u05E8\u05D9\u05DD \u05E9\
  \u05D4\u05D5\u05E2\u05D1\u05E8\u05D5 \u05DC\u05EA\u05D5\u05DB\u05E0\u05D9\u05EA\
  \ \u05E9\u05DC\u05DA \u05D1\u05E2\u05EA \u05D4\u05D4\u05E4\u05E2\u05DC\u05D4. \u05D4\
  \u05D8\u05DB\u05E0\u05D9\u05E7\u05D4 \u05D4\u05D6\u05D5 \u05DE\u05E9\u05DE\u05E9\
  \u05EA \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD \u05E7\u05E8\u05D5\u05D1\u05D5\u05EA\
  \u2026"
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05D0\u05E8\u05D2\u05D5\u05DE\u05E0\u05D8\u05D9\
  \u05DD \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4"
---

{{< edit_this_page >}}

## מה ולמה?

קריאת ארגומנטים של שורת הפקודה ב-Visual Basic for Applications (VBA) כוללת גישה לפרמטרים שהועברו לתוכנית שלך בעת ההפעלה. הטכניקה הזו משמשת לעיתים קרובות להשפיע על התנהגות או פלט של תוכנית ללא הצורך באינטרקציה מצד המשתמש, מה שהופך משימות של אוטומציה וכתיבת סקריפטים להרבה יותר פשוטות וגמישות.

## איך לעשות:

בניגוד לסביבות תכנות יותר פשוטות, ל-VBA אין תכונה מובנית לקרוא ארגומנטים של שורת פקודה בחוש אקונוונציונלי מפני שהוא מיועד בעיקר להטמעה בתוך יישומי Microsoft Office. עם זאת, עם קצת יצירתיות, אנו יכולים להשתמש ב-Windows Script Host (WSH) או לקרוא ל-APIs חיצוניים כדי להשיג פונקציונליות דומה. הנה פתרון פרקטי באמצעות WSH:

1. **יצירת VBScript להעברת ארגומנטים ל-VBA:**

   תחילה, כתבו קובץ VBScript (*yourScript.vbs*) שמפעיל את היישום VBA שלכם (למשל, מאקרו של Excel) ומעביר את ארגומנטי שורת הפקודה:

```vb
Set objExcel = CreateObject("Excel.Application")
objExcel.Workbooks.Open "C:\YourMacroWorkbook.xlsm"
objExcel.Run "YourMacroName", WScript.Arguments.Item(0), WScript.Arguments.Item(1)
objExcel.Quit
```

2. **גישה לארגומנטים ב-VBA:**

   ביישום VBA שלכם (*YourMacroWorkbook.xlsm*), שנו או צרו מחדש את המאקרו (*YourMacroName*) כך שיקבל פרמטרים:

```vb
Sub YourMacroName(arg1 As String, arg2 As String)
    MsgBox "ארגומנט 1: " & arg1 & " ארגומנט 2: " & arg2
End Sub
```

3. **הפעלת הסקריפט שלכם:**

   הפעילו את ה-VBScript משורת הפקודה, תוך העברת הארגומנטים כפי הצורך:

```shell
cscript yourScript.vbs "שלום" "עולם"
```

   זה אמור להוביל לביצוע המאקרו VBA שלכם עם הארגומנטים "שלום" ו"עולם", ולהציג אותם בתיבת הודעה.

## חקירה מעמיקה:

בהקשר ההיסטורי, VBA נועד להרחיב את יכולות היישומים של Microsoft Office, ולא כסביבת תכנות עצמאית. כתוצאה מכך, אינטרקציה ישירה עם שורת הפקודה היא מחוץ לתחום העיקרי שלו, מה שמסביר את היעדר התמיכה המובנית לקריאת ארגומנטים של שורת פקודה.

השיטה שהוצגה לעיל, אף על פי שהיא יעילה, היא יותר פתרון זמני מאשר פתרון מובנה, שמנצלת סקריפטינג חיצוני כדי לגשר על הפער. גישה זו יכולה להכניס סיבוכיות ודאגות פוטנציאליות לאבטחה, מכיוון שהיא דורשת הפעלה של מאקרואים ולעיתים קרובות ירידה בהגדרות האבטחה כדי להריץ.

למשימות שתלויות באופן רב בארגומנטים של שורת פקודה או שזקוקות לאינטגרציה חלקה יותר עם מערכת ההפעלה של Windows, שפות תכנות אחרות כמו PowerShell או Python עשויות להציע פתרונות יותר חזקים ובטוחים. האלטרנטיבות הללו מספקות תמיכה ישירה בארגומנטים של שורת פקודה והן מתאימות יותר ליישומים או סקריפטים עצמאיים שדורשים קלט חיצוני כדי לשנות את התנהגותם בצורה דינמית.
