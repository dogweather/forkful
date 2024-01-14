---
title:                "Kotlin: יצירת קובץ זמני"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

##למה

יצירת קובץ זמני בקוד נדרש כאשר יש לנו צורך ליצור קובץ באופן זמני ולא לעולם לשמור עליו באופן קבוע. הקבצים הזמניים עשויים לשמש במגוון מטרות, כגון בדיקות יחידה, ניסיון פתרון בעיות, או כאמצעי לשמירת נתונים בזמן ריצת התוכנית. בכתבה זו נלמד כיצד ליצור ולנהל קבצים זמניים בשפת קוטלין.

##איך לעשות זאת

כדי ליצור קובץ זמני בקוד קוטלין, נשתמש בתוכנה לניהול קבצים המכונה "TemporaryFile". נתחיל על ידי הוספת התלות הבאה לקובץ ה- build.gradle:

```
dependencies {
    implementation("com.google.guava:guava:30.1.1-jre")
}
```

לאחר מכן, נגדיר משתנה לכתיבת קובץ זמני בעזרת פקודת "createTempFile":

```
val tempFile = Files.createTempFile("prefix", "suffix")
```

נוכל לשנות את התצורה של הקובץ הזמני על ידי שימוש בפרמטרים נוספים כגון: "directory" - להגדרה של התיקייה בה ייוצר הקובץ הזמני, "charset" - להגדרת קידוד התווים של הקובץ, ועוד.

ניתן לכתוב נתונים לקובץ באמצעות פעולת "writeText" ולקרוא ממנו באמצעות "readText".

```
// כתיבת נתונים לקובץ זמני
tempFile.writeText("כתוב כאן את המילות שאתה צריך.")
// קריאת נתונים מהקובץ זמני
println(tempFile.readText())
```

למחיקת הקובץ הזמני, נשתמש בפעולה "delete":

```
tempFile.delete()
```

##צילום עמוק

בדרך כלל, קבצים זמניים נשמרים בקישור של קובץ מסוג "java.io.File", בעוד שבקוטלין הם נלכדים במחלקת "java.lang.AutoCloseable", המיועדת לטיפול במחלקות המק