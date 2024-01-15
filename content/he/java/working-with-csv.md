---
title:                "עבודה עם CSV"
html_title:           "Java: עבודה עם CSV"
simple_title:         "עבודה עם CSV"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/working-with-csv.md"
---

{{< edit_this_page >}}

## מדוע

חלק גדול מהנתונים במחשב ניתנים לאחסון בתוך טקסט פשוט, ואחת הפורמטים הנפוצים הוא CSV (Comma Separated Values). בפורמט זה, כל הנתונים מופרדים על ידי פסיקים, וכך ניתן לקרוא ולכתוב את הנתונים בקלות. במאמר זה נלמד כיצד לעבוד עם נתונים בפורמט CSV בשפת ג'אווה, ונראה מדוע זה יכול להיות שימושי לכם.

## איך לעבוד עם CSV בשפת ג'אווה

ראשית, ניתן לקרוא נתונים מקובץ CSV באמצעות הספרייה המובנית "CSVReader" מחברת "OpenCSV". הראשון נצטרך להוריד את הספרייה הזו ולהוסיף אותה לפרוייקט שלנו. לדוגמה, אם נרצה לקרוא נתונים מקובץ בשם "data.csv", נוכל לעשות כך באמצעות המקוד הבא:

```Java
CSVReader reader = new CSVReader(new FileReader("data.csv"));
String[] line;

while ((line = reader.readNext()) != null) {
    // הורדת הנתונים מהשורה והשמתם במשתנה מסוג מערך מחרוזות
    // כאן תוכלו לבצע כל פעולות אחרות על הנתונים
}
```

ניתן גם לכתוב נתונים לקובץ CSV על ידי שימוש בספרייה זו. לדוגמה, נרצה ליצור קובץ חדש בשם "output.csv" ולרשום לתוכו את כל הנתונים ממערך המחרוזות "data":

```Java
CSVWriter writer = new CSVWriter(new FileWriter("output.csv"));
writer.writeNext(data);
writer.close();
```

כעת נוכל לפתוח את הקובץ "output.csv" ולראות שהנתונים נכתבו בפורמט CSV כראוי.

## העמקה

כאשר מתעסקים עם נתונים בפורמט CSV, חשוב לבדוק שהנתונים מכילים את ההידורים הנכונים וכי הם מופרדים בצורה תקינה. ניתן להשתמש בספרי