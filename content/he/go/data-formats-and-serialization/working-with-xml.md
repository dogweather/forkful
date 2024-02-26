---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:14:06.264402-07:00
description: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML \u05D1\u05D2\u05D5 \u05DB\
  \u05D5\u05DC\u05DC\u05EA \u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 (\u05E7\u05E8\u05D9\
  \u05D0\u05D4) \u05D5\u05D9\u05D9\u05E6\u05D5\u05E8 (\u05DB\u05EA\u05D9\u05D1\u05D4\
  ) \u05DE\u05E1\u05DE\u05DB\u05D9 XML\u2014\u05E4\u05D5\u05E8\u05DE\u05D8 \u05EA\u05E7\
  \u05E0\u05D9 \u05E2\u05D1\u05D5\u05E8 \u05D4\u05D7\u05DC\u05E4\u05EA \u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD \u05DE\u05D5\u05D1\u05E0\u05D9\u05DD. \u05DE\u05EA\u05DB\
  \u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DC\
  \u05E9\u05DD \u05D0\u05D7\u05E1\u05D5\u05DF \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\
  , \u05D4\u05D2\u05D3\u05E8\u05D5\u05EA\u2026"
lastmod: '2024-02-25T18:49:36.845859-07:00'
model: gpt-4-0125-preview
summary: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML \u05D1\u05D2\u05D5 \u05DB\
  \u05D5\u05DC\u05DC\u05EA \u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 (\u05E7\u05E8\u05D9\
  \u05D0\u05D4) \u05D5\u05D9\u05D9\u05E6\u05D5\u05E8 (\u05DB\u05EA\u05D9\u05D1\u05D4\
  ) \u05DE\u05E1\u05DE\u05DB\u05D9 XML\u2014\u05E4\u05D5\u05E8\u05DE\u05D8 \u05EA\u05E7\
  \u05E0\u05D9 \u05E2\u05D1\u05D5\u05E8 \u05D4\u05D7\u05DC\u05E4\u05EA \u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD \u05DE\u05D5\u05D1\u05E0\u05D9\u05DD. \u05DE\u05EA\u05DB\
  \u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DC\
  \u05E9\u05DD \u05D0\u05D7\u05E1\u05D5\u05DF \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\
  , \u05D4\u05D2\u05D3\u05E8\u05D5\u05EA\u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML"
---

{{< edit_this_page >}}

## מה ולמה?

עבודה עם XML בגו כוללת פיענוח (קריאה) וייצור (כתיבה) מסמכי XML—פורמט תקני עבור החלפת נתונים מובנים. מתכנתים עושים זאת לשם אחסון נתונים, הגדרות קונפיגורציה, או החלפת נתונים בין מערכות, במיוחד בסביבות שבהן XML הוא הפורמט המועדף או הישן.

## איך לעשות:

### פיענוח XML בגו
כדי לפענח XML בגו, אתם משתמשים בחבילת `encoding/xml`. חבילה זו מספקת את הכלים הדרושים לבצע אנמרשל (פיענוח) של XML לתוך מבני נתונים של גו. לדוגמה, שקלו את נתוני ה-XML הבאים שמייצגים ספר:

```xml
<book id="123">
    <title>Learning Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
```

כדי לפענח את זה, הגדרו מבנה שמשקף את מבנה ה-XML:

```go
package main

import (
    "encoding/xml"
    "fmt"
    "os"
)

type Book struct {
    XMLName xml.Name `xml:"book"`
    ID      string   `xml:"id,attr"`
    Title   string   `xml:"title"`
    Author  string   `xml:"author"`
    Pages   int      `xml:"pages"`
}

func main() {
    data := []byte(`
<book id="123">
    <title>Learning Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
`)

    var book Book
    err := xml.Unmarshal(data, &book)
    if err != nil {
        panic(err)
    }

    fmt.Printf("Book: %+v\n", book)
}
```

פלט:

```
Book: {XMLName:{Space: Local:book} ID:123 Title:Learning Go Author:John Doe Pages:359}
```

### יצירת XML בגו
כדי לייצר מסמך XML ממבני נתונים של גו, שוב אתם משתמשים בחבילת `encoding/xml`. הפעם אתם מבצעים מרשלינג של מבני גו לתוך XML. בהינתן מבנה ה-`Book` הקודם:

```go
package main

import (
    "encoding/xml"
    "fmt"
    "os"
)

func main() {
    book := &Book{
        ID:     "123",
        Title:  "Learning Go",
        Author: "John Doe",
        Pages:  359,
    }

    output, err := xml.MarshalIndent(book, "", "    ")
    if err != nil {
        panic(err)
    }

    fmt.Println(xml.Header + string(output))
}
```

פלט:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<book id="123">
    <title>Learning Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
```

## צלילה עמוקה

המורכבות והמילוליות של XML הביאו לכך ש-JSON ופורמטים אחרים הפכו לפופולריים יותר עבור רבים מהיישומים. עם זאת, היכולת של XML לייצג נתונים היררכיים מורכבים והשימוש הנרחב בו במערכות ישנות ותחומים ספציפיים (למשל, שירותי SOAP) מבטיחים את שיווי המשקל שלו.

חבילת ה-`encoding/xml` בגו מספקת מנגנונים חזקים לעבודה עם XML, אך כדאי לזכור את ההגבלות שלה. לדוגמה, טיפול במרחבי שמות XML יכול להיות מסורבל ועלול לדרוש הבנה מפורטת יותר של התקן ה-XML לעומת מקרי שימוש פשוטים יותר. נוסף על כך, על אף שההקלדה הסטטית של גו ויכולות המרשלינג והאנמרשלינג של חבילת ה-`encoding/xml` בדרך כלל יעילות, מפתחים עשויים להתמודד עם אתגרים במבנים מקוננים במיוחד או כאשר מתמודדים עם מסמכי XML שלא מתאימים בצורה נקייה למערכת הטיפוסים של גו.

לרוב היישומים המודרניים, אלטרנטיבות כמו JSON פשוטות ויעילות יותר. עם זאת, כאשר עובדים בהקשרים שדורשים XML—בשל מערכות ישנות, תקנים תעשייתיים מסוימים, או צרכי ייצוג נתונים מורכבים—ספריית התקנים של גו מספקת כלים חזקים להשגת המטרה. כמו תמיד, הבחירה הטובה ביותר של פורמט הנתונים תלויה בדרישות הספציפיות של היישום והסביבה.
