---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:47.649579-07:00
description: "\u05D0\u05D9\u05DA \u05DC: \u05DC-Java \u05D0\u05D9\u05DF \u05EA\u05DE\
  \u05D9\u05DB\u05D4 \u05DE\u05D5\u05D1\u05E0\u05D9\u05EA \u05DC-CSV \u05D1\u05E1\u05E4\
  \u05E8\u05D9\u05D9\u05EA\u05D4 \u05D4\u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9\u05EA\
  , \u05D0\u05DA \u05E0\u05D9\u05EA\u05DF \u05D1\u05E7\u05DC\u05D5\u05EA \u05DC\u05E7\
  \u05E8\u05D5\u05D0 \u05E7\u05D5\u05D1\u05E5 CSV \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\
  \u05EA \u05DE\u05D7\u05DC\u05E7\u05D5\u05EA `java.io`."
lastmod: '2024-03-13T22:44:39.169087-06:00'
model: gpt-4-0125-preview
summary: "\u05DC-Java \u05D0\u05D9\u05DF \u05EA\u05DE\u05D9\u05DB\u05D4 \u05DE\u05D5\
  \u05D1\u05E0\u05D9\u05EA \u05DC-CSV \u05D1\u05E1\u05E4\u05E8\u05D9\u05D9\u05EA\u05D4\
  \ \u05D4\u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9\u05EA, \u05D0\u05DA \u05E0\u05D9\
  \u05EA\u05DF \u05D1\u05E7\u05DC\u05D5\u05EA \u05DC\u05E7\u05E8\u05D5\u05D0 \u05E7\
  \u05D5\u05D1\u05E5 CSV \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05DE\u05D7\u05DC\
  \u05E7\u05D5\u05EA `java.io`."
title: "\u05E2\u05D5\u05D1\u05D3\u05D9\u05DD \u05E2\u05DD CSV"
weight: 37
---

## איך ל:


### קריאת קובץ CSV באמצעות ספריית Java הסטנדרטית
ל-Java אין תמיכה מובנית ל-CSV בספרייתה הסטנדרטית, אך ניתן בקלות לקרוא קובץ CSV באמצעות מחלקות `java.io`.

```java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class ReadCSVExample {
    public static void main(String[] args) {
        String line;
        String csvFile = "data.csv"; // ציין את הנתיב לקובץ CSV
        try (BufferedReader br = new BufferedReader(new FileReader(csvFile))) {
            while ((line = br.readLine()) != null) {
                String[] values = line.split(","); // בהנחה שפסיק הוא המפריד
                // עיבוד הנתונים
                for (String value : values) {
                    System.out.print(value + " ");
                }
                System.out.println();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

### כתיבה לקובץ CSV באמצעות ספריית Java הסטנדרטית
לכתיבת נתונים לקובץ CSV, ניתן להשתמש במחלקות `java.io` כמו `FileWriter` ו-`BufferedWriter`.

```java
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class WriteCSVExample {
    public static void main(String[] args) {
        String[] data = {"John", "Doe", "30", "New York"};
        String csvFile = "output.csv"; // ציון נתיב קובץ CSV היציאה

        try (BufferedWriter bw = new BufferedWriter(new FileWriter(csvFile))) {
            StringBuilder sb = new StringBuilder();
            for (String value : data) {
                sb.append(value).append(","); // בהנחה שפסיק הוא המפריד
            }
            sb.deleteCharAt(sb.length() - 1); // הסרת הפסיק האחרון
            bw.write(sb.toString());
            bw.newLine();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

### שימוש בספרייה מצד שלישי: Apache Commons CSV
Apache Commons CSV היא ספרייה פופולרית לטיפול בקבצי CSV ב-Java. היא מפשטת משמעותית את הקריאה והכתיבה של קבצי CSV.

הוספת התלות לפרויקט שלך:

עבור Maven:

```xml
<dependency>
    <groupId>org.apache.commons</groupId>
    <artifactId>commons-csv</artifactId>
    <version>1.9.0</version> <!-- בדוק את הגרסה האחרונה -->
</dependency>
```

#### קריאת קובץ CSV:
```java
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;

import java.io.Reader;
import java.io.FileReader;
import java.io.IOException;

public class ApacheReadCSVExample {
    public static void main(String[] args) {
        String csvFile = "data.csv";
        try (Reader reader = new FileReader(csvFile);
             CSVParser csvParser = new CSVParser(reader, CSVFormat.DEFAULT)) {
            for (CSVRecord csvRecord : csvParser) {
                // גישה לערכים לפי אינדקסי העמודות
                String columnOne = csvRecord.get(0);
                String columnTwo = csvRecord.get(1);
                System.out.println(columnOne + " " + columnTwo);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

#### כתיבה לקובץ CSV:
```java
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class ApacheWriteCSVExample {
    public static void main(String[] args) {
        String[] headers = {"שם פרטי", "שם משפחה", "גיל", "עיר"};
        String[] data = {"John", "Doe", "30", "New York"};

        try (BufferedWriter writer = new BufferedWriter(new FileWriter("output.csv"));
             CSVPrinter csvPrinter = new CSVPrinter(writer, CSVFormat.DEFAULT.withHeader(headers))) {
            csvPrinter.printRecord((Object[]) data); // יציקה ל-Object[] הכרחית כאן
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Apache Commons CSV טופלת במורכבויות כמו ציטוטים ופסיקים בתוך שדות באופן אוטומטי, והופכת אותה לבחירה עמידה לניפוי CSV ב-Java.
