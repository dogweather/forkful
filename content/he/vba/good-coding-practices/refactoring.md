---
title:                "שיפור קוד"
aliases: - /he/vba/refactoring.md
date:                  2024-02-01T22:01:02.709896-07:00
model:                 gpt-4-0125-preview
simple_title:         "שיפור קוד"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/vba/refactoring.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

ריפקטורינג בתכנות כולל שינויים במבנה הקוד ללא שינוי בהתנהגותו, על מנת לשפר אספקטים כמו קריאות, ניתנות לתחזוקה, או ביצועים. תכניתנים מבצעים ריפקטורינג כדי להפוך קוד ליעיל יותר, קל יותר להבנה, קל יותר לשינוי בעתיד, וכדי להפחית את הסבירות לבאגים.

## איך לעשות:

בואו נשקול דוגמה בסיסית ב-Visual Basic for Applications (VBA) שבה יש לנו פרוצדורת תת שמדפיסה פרטים על עובד. בהתחלה, הקוד צפוף, אתגרי לתחזוקה או להרחבה.

```vb
Sub PrintEmployeeDetails()
    Dim name As String
    Dim age As Integer
    Dim department As String
    name = "John Doe"
    age = 30
    department = "IT"
    
    MsgBox "Name: " & name & vbCrLf & "Age: " & age & vbCrLf & "Department: " & department
End Sub
```

שלב ריפקטורינג 1: חילוץ שיטה. אחת מהטכניקות הנפוצות ביותר בריפקטורינג היא לקחת חלק ספציפי מהקוד ולהעביר אותו לשיטה משלו. זה הופך את הקוד למודולרי יותר וקל יותר להבנה.

```vb
Sub PrintEmployeeDetails()
    Dim name As String
    Dim age As Integer
    Dim department As String
    name = "John Doe"
    age = 30
    department = "IT"
    
    DisplayMessage name, age, department
End Sub

Private Sub DisplayMessage(name As String, age As Integer, department As String)
    MsgBox "Name: " & name & vbCrLf & "Age: " & age & vbCrLf & "Department: " & department
End Sub
```

שלב ריפקטורינג 2: שימוש במבנה. שלב זה כולל שימוש במבנה נתונים כדי להחזיק נתונים קשורים, שיפור בהירות הקוד והפיכתו לקל יותר להעברת נתונים מקובצים.

```vb
Type Employee
    name As String
    age As Integer
    department As String
End Type

Sub PrintEmployeeDetails()
    Dim emp As Employee
    emp.name = "John Doe"
    emp.age = 30
    emp.department = "IT"
    
    DisplayMessage emp
End Sub

Private Sub DisplayMessage(emp As Employee)
    MsgBox "Name: " & emp.name & vbCrLf & "Age: " & emp.age & vbCrLf & "Department: " & emp.department
End Sub
```

השלבים האלו משנים קוד צפוף לקוד מודולרי וממובנה, שיפור משמעותי בקריאות ובניתנות לתחזוקה.

## צלילה עמוקה

המושג של ריפקטורינג כמעט ולא חדש בתכנות, אך זה הספר של מרטין פולר, "ריפקטורינג: שיפור עיצוב הקוד הקיים", שהביא אותו לזירה הראשית, והדגיש את חשיבותו בתהליך פיתוח התוכנה. ב-Visual Basic for Applications, ריפקטורינג יכול להיות מאתגר יותר בשל החסרון בכלים מובנים שנמצאים בסביבות פיתוח משולבות (IDEs) יותר מודרניות שתומכות בריפקטורינג אוטומטי.

עם זאת, זה לא מקטין את חשיבותו. גם ב-VBA, החלת טכניקות ריפקטורינג בסיסיות באופן ידני יכולה לשפר באופן משמעותי את מאגר הקוד, להפוך אותו לנקי יותר ויעיל יותר. אף על פי ש-VBA אולי לא מכיל את הנוחויות המודרניות כמו שפות אחרות, עקרונות עיצוב הקוד הטובים נשארים אוניברסליים. מפתחים הבאים משפות אחרות עשויים למצוא את התהליך הידני מעייף אך בוודאי יעריכו את היתרונות של השקעת זמן בשיפור איכות הקוד מההתחלה.

לסביבות פיתוח חזקות יותר או כאשר עובדים על פרויקטים מתוחכמים במיוחד, יתכן ויהיה שווה לחקור אלטרנטיבות שמציעות כלים חזקים יותר לריפקטורינג או להמיר פרויקטי VBA לשפה של .NET שבה Visual Studio מספקת תמיכה נרחבת בריפקטורינג. עם זאת, הבנה ויישום של עקרונות ריפקטורינג ב-VBA היא מיומנות יקרת ערך המדגישה את חשיבות כתיבת קוד נקי וניתן לתחזוקה, ללא קשר לסביבה.
