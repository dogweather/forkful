---
title:                "C#: כתיבת מבחנים"
simple_title:         "כתיבת מבחנים"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

# מדוע

ברכישה לכתוב בדיקות יש לפתוח קוד שבו ניתן לבצע בדיקה על הקוד בכדי לוודא שהוא עובד תקין. כתיבת בדיקות מאפשרת למתכנת להיות בטוח שהקוד שהוא כותב עובד כפי שצריך וכן מאפשר לו לזהות בעיות ולתקן אותן בכתיבת הקוד.

# איך לכתוב בדיקות בסגנון C#

כדי לכתוב בדיקות בסגנון C# ניתן לעקוב אחר השלבים הבאים:

- צור כיתת מבדק עבור הקוד שתרצה לבדוק.
- הגדר משתנים וצור פונקציות שתכתב את הקוד שתשתמש בעת בדיקת הקוד.
- הרץ את הפונקציות ובדוק את התוצאות כדי לוודא שהקוד פועל כצפוי.
- יש לכתוב מעקב אחרי הבדיקות ולהפעיל אותן בכל פעם שנרצה לבדוק שהקוד עדיין עובד כראוי.

קוד דוגמה:

```C#
// ייצוא משתנים מתוך המשתנים הנכונים ותמיד מתחילים מ- Test
[Test]
public void Export_Data_Checks_Variables()
{
    DataExporter exporter = new DataExporter();
    exporter.ExportData();
    Assert.AreEqual(10, exporter.TotalData);
    Assert.IsTrue(exporter.IsExported);
}
```

תוצאת המתקדמת:

```C#
TotalData: 10
IsExported: true
```

# העמקה

כתיבת בדיקות היא חלק מאוד חשוב בתכנות כי היא מאפשרת למתכנת לזהות בעיות כבר בשלב המבנה ולאחר מכן לתקן אותן. בנוסף, כתיבת בדיקות מסייעת בתחזוקת הקוד וביכולת להתאים אותו לצרכי המקוריים.

בנוסף לכתיבת בדיקות פרטיות, יתר על כן מומלץ להשתמש בכלי Automate Testing כמו Nunit כדי לאפשר ללא עבורכם לאכת בדיקות באופן אוטומטי.

# ראו גם

- [Nunit על GitHub](https://github.com/n