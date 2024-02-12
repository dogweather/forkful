---
title:                "עבודה עם XML"
aliases:
- he/vba/working-with-xml.md
date:                  2024-02-01T22:08:04.629350-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם XML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/vba/working-with-xml.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

עבודה עם XML ב-Visual Basic for Applications (VBA) כוללת ניתוח, יצירה, ושינוי מסמכי XML בהקשר של אפליקציות Microsoft Office. מתכנתים פונים ליכולת זו לשם השתלבות אפליקציות Office עם שירותי אינטרנט או מקורות נתונים אחרים המפיקים XML, וזאת כדי לקדם חליפין של נתונים ולהקל על פעולות דיווח.

## איך ל:

כדי להתחיל להתעסק עם XML, בדרך כלל משתמשים באובייקט `MSXML2.DOMDocument`. ממשק זה מאפשר לך לטעון, לנתח, ולנווט במסמכי XML. להלן דוגמה פשוטה המדגימה איך לטעון קובץ XML, לנווט במבנה שלו, ולקרוא מאפיינים ותוכן טקסטואלי.

```basic
' ראשית, וודא שהוספת את ההפנייה ל-"Microsoft XML, v6.0" דרך Tools -> References
Dim xmlDoc As MSXML2.DOMDocument60
Set xmlDoc = New MSXML2.DOMDocument60
xmlDoc.async = False
xmlDoc.Load("C:\Path\To\Your\File.xml") ' טען את קובץ ה-XML שלך

' בדוק אם ה-XML נטען בהצלחה
If xmlDoc.parseError.ErrorCode <> 0 Then
    MsgBox "Error loading XML:" & xmlDoc.parseError.reason
Else
    ' נווט וקרא אלמנטים
    Dim book As IXMLDOMNode
    Set book = xmlDoc.SelectSingleNode("//book/title") ' XPath למציאת ה-<title> הראשון בתוך <book>
    MsgBox book.Text ' הצג את טקסט הכותרת
End If
```

בקוד הדוגמה לעיל, אנו יוצרים מופע של `MSXML2.DOMDocument60`, טוענים קובץ XML, ולאחר מכן בודקים אם יש שגיאות. אם לא נמצאו שגיאות, אנו נוהגים לצומת ספציפי באמצעות XPath ומציגים את תוכן הטקסט שלו.

## צלילה עמוקה:

התמזגות היכולות של XML ב-VBA התחילה בראשית שנות ה-2000, כשהצורך של אפליקציות Office להתקשר עם נתונים ושירותים מהאינטרנט התחיל לגדול. הספרייה `MSXML`, או שירותי הליבה של Microsoft XML, התפתחה לאורך השנים, כאשר `MSXML2.DOMDocument60` הוא אחד מהגרסאות החדשות הממולצות לשימוש בשל תכונות הביצועים והאבטחה המשופרות שלו.

למרות שהם עוצמתיים, יכולות ההתמודדות עם XML של VBA נחשבות לפחות יעילות ויותר מסורבלות בהשוואה לסביבות תכנות מודרניות כמו XML.etree של Python או LINQ to XML של C#. המסורבלות הטבעית של VBA והצורך להוסיף ולנהל הפניות באופן ידני יכולים להפחית את הפיתוח המהיר. יתרה מכך, עם זרחנה של JSON כפורמט החלפת נתונים קל משקל יותר, רבים מהתכנתים והאפליקציות עוברים מ-XML, למעט במקרים בהם נדרשת אינטרופרביליות עם מערכות ישנות או שירותים ארגוניים ספציפיים.

עם זאת, למשימות הדורשות ניתוח או יצירת מסמכי XML בהקשר של אוטומציה של אפליקציות Microsoft Office, השתמשות בתכונות העיבוד של XML של VBA עדיין נשארת גישה נחוצה ולעיתים רחוקות הכרחית. זה יוצר איזון בין גישה לערכת התכונות העשירה של אפליקציות Office ויכולות הניפוי של נתונים מובנים שXML מספק.
