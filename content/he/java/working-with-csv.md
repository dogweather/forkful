---
title:                "Java: עבודה עם קובץ csv"
simple_title:         "עבודה עם קובץ csv"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/working-with-csv.md"
---

{{< edit_this_page >}}

##Why (למה):

CSV הוא פורמט נתונים נפוץ ביותר בעולם התכנות. הוא מאפשר יצירת טבלאות נתונים פשוטות בקלות והינו תואם למגוון רחב של יישומים. במאמר זה, אנו נכיר כיצד לעבוד עם קבצי CSV בשפת ג'אווה ונראה כמה דוגמאות פשוטות להשתמש בשנים.

##How To (כיצד לעבוד עם CSV):

כדי לעבוד עם קבצי CSV בג'אווה, ניתן לעזור בספריית OpenCSV. נתחיל על ידי החברת את הספרייה לפרויקט שלנו וניצור אובייקט לכתיבת קובץ CSV עם תכונות מתאימות. לדוגמא:

```Java
CSVWriter writer = new CSVWriter(new FileWriter("output.csv"), ';' , CSVWriter.NO_ESCAPE_CHARACTER, CSVWriter.DEFAULT_LINE_END);
```

לאחר מכן, נוסיף שורות לקובץ על ידי השימוש בפונקציה `writeNext()` ונסגור את האובייקט כדי להשלים את התהליך.

```Java
writer.writeNext(new String[]{"ID", "שם", "גיל"});
writer.writeNext(new String[]{"1", "טלי", "25"});
writer.close();
```

להדפיס קובץ CSV קיים, ניתן להשתמש בעזרת הפונקציה `parse()` כדי לקרוא את הקובץ ולהדפיס את הנתונים. דוגמא:

```Java
CSVReader reader = new CSVReader(new FileReader("sample.csv"));
String[] line;
while ((line = reader.readNext()) != null) {
   System.out.println("ID: " + line[0] + ", שם: " + line[1] + ", גיל: " + line[2]);
}
reader.close(); 
```

##Deep Dive (לרדת לעומק):

בנוסף לפונקציות הבסיסיות של OpenCSV, ישנן אפשרויות נוספות להתאים את הפעולה לצרכים המיוחדים שלנו. לדוגמא, אנו יכולים להגדיר תכונות נוספות כמו סימון נכון ושגיאות, כתיבת טבלאות בשפת JSON ועוד. ניתן למצוא מידע מפורט על כל האפשרויות בדוקומנטציית OpenCSV הרשמית.

##See Also (ראו