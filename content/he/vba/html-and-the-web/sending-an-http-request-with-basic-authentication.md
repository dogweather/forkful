---
aliases:
- /he/vba/sending-an-http-request-with-basic-authentication/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:03:31.948631-07:00
description: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9 \u05D1-Visual Basic\
  \ for Applications (VBA) \u05E7\u05E9\u05D5\u05E8\u05D4 \u05DC\u05D2\u05D9\u05E9\
  \u05D4 \u05DC\u05DE\u05E9\u05D0\u05D1\u05D9 \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\
  \u05D8 \u05D4\u05DE\u05D5\u05D2\u05E0\u05D9\u05DD \u05E2\u05DC \u05D9\u05D3\u05D9\
  \ \u05E9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9 \u05D5\u05E1\u05D9\u05E1\u05DE\u05D4\
  . \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\
  \u05D0\u05EA \u05DB\u05D3\u05D9\u2026"
lastmod: 2024-02-18 23:08:52.661259
model: gpt-4-0125-preview
summary: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9 \u05D1-Visual Basic\
  \ for Applications (VBA) \u05E7\u05E9\u05D5\u05E8\u05D4 \u05DC\u05D2\u05D9\u05E9\
  \u05D4 \u05DC\u05DE\u05E9\u05D0\u05D1\u05D9 \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\
  \u05D8 \u05D4\u05DE\u05D5\u05D2\u05E0\u05D9\u05DD \u05E2\u05DC \u05D9\u05D3\u05D9\
  \ \u05E9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9 \u05D5\u05E1\u05D9\u05E1\u05DE\u05D4\
  . \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\
  \u05D0\u05EA \u05DB\u05D3\u05D9\u2026"
title: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9"
---

{{< edit_this_page >}}

## מה ולמה?

שליחת בקשת HTTP עם אימות בסיסי ב-Visual Basic for Applications (VBA) קשורה לגישה למשאבי אינטרנט המוגנים על ידי שם משתמש וסיסמה. מתכנתים עושים זאת כדי להתממשק עם ממשקי API מאובטחים או שירותי רשת בתוך היישומים שלהם המופעלים על ידי VBA, כגון אוטומציה של משימות ב-Excel או Access עם נתונים מנקודות קצה מאובטחות.

## איך לעשות:

ב-VBA, ניתן להשתמש בספריית `Microsoft XML, v6.0` (MSXML2) כדי לשלוח בקשות HTTP עם אימות בסיסי. זה כולל הגדרת הכותרת `"Authorization"` של הבקשה כך שתכלול את נתוני האימות בפורמט מקודד base64. הנה מדריך צעד אחר צעד:

1. **הפניה ל-MSXML2**: תחילה, וודאו שהפרוייקט שלכם ב-VBA מפנה לספריית `Microsoft XML, v6.0`. בעורך ה-VBA, עברו ל-Tools > References וסמנו `Microsoft XML, v6.0`.

2. **יצירה ושליחת בקשת HTTP**: השתמשו בקטע הקוד של VBA הבא כמדריך. החליפו את `"your_username"` ו-`"your_password"` עם נתוני האימות שלכם והתאימו את ה-URL לפי הצורך.

   ```vb
    Dim XMLHttp As Object
    Set XMLHttp = CreateObject("MSXML2.XMLHTTP")
    Dim url As String
    url = "http://example.com/api/resource" ' החליפו עם ה-URL האמיתי
    Dim base64Credentials As String
    base64Credentials = EncodeBase64("your_username:your_password")
    
    XMLHttp.Open "GET", url, False
    XMLHttp.setRequestHeader "Authorization", "Basic " & base64Credentials
    XMLHttp.send
    
    Debug.Print XMLHttp.responseText ' מדפיס את התגובה לחלון המיידי
    ```

3. **קידוד האימות ב-base64**: ל-VBA אין פונקציה מובנית לקידוד ב-base64, אך ניתן להשתמש בפונקציה המותאמת אישית הזו, `EncodeBase64`:

    ```vb
    Function EncodeBase64(text As String) As String
        Dim arrData() As Byte
        arrData = StrConv(text, vbFromUnicode)
        
        Dim objXML As MSXML2.DOMDocument60
        Dim objNode As MSXML2.IXMLDOMElement
        
        Set objXML = New MSXML2.DOMDocument60
        Set objNode = objXML.createElement("b64")
        
        objNode.dataType = "bin.base64"
        objNode.nodeTypedValue = arrData
        EncodeBase64 = objNode.Text
    End Function
    ```

זה ישלח בקשת GET ל-`http://example.com/api/resource` עם נתוני האימות הבסיסיים המצוינים, וידפיס את התגובה.

## עיון נוסף

הגישה המשמשת כאן, אף על פי שהיא יעילה למקרי שימוש פשוטים, נשענת על תכנית האימות הבסיסית, ששולחת נתוני אימות בפורמט שניתן לפענוח בקלות (קידוד ב-base64 אינו הצפנה). בשל פגיעותה, במיוחד בהקשרים לא מאובטחים ב-HTTPS, אימות בסיסי אינו מומלץ להעברת נתונים רגישים באינטרנט ללא שכבות אבטחה נוספות כמו SSL/TLS.

באופן היסטורי, אימות בסיסי היה אחת השיטות הראשונות שפותחו לשליטה על גישה למשאבי אינטרנט. כיום, תקני אימות בטוחים וגמישים יותר, כמו OAuth 2.0, מועדפים בדרך כלל ליישומים חדשים. בהתחשב במגבלות של VBA ובתלות החיצונית הנדרשת לשיטות אימות מתקדמות יותר, מתכנתים לעיתים קרובות משתמשים ב-VBA בסביבות פנימיות או פחות קריטיות מבחינת אבטחה, או משתמשים בו כדי להקדים רעיונות במהירות.

כאשר משתמשים ב-VBA לבקשות HTTP, זכרו שכל גרסה של ספריית MSXML עשויה לתמוך בתכונות ובתקני אבטחה שונים. השתמשו תמיד בגרסה העדכנית ביותר התואמת ליישום שלכם כדי להבטיח אבטחה וביצועים טובים יותר. בנוסף, שקלו את המגבלות הסביבתיות והתכונות שעשויות להיחשב למיושנות כאשר בוחרים ב-VBA לפרויקטים חדשים, במיוחד אלו הדורשים תקשורת HTTP מאובטחת. סביבות תכנות או שפות תכנות אחרות עשויות להציע פתרונות יציבים, בטוחים וקלים לתחזוקה יותר למשימות דומות.
