---
title:                "עבודה עם XML"
date:                  2024-02-03T18:14:06.264402-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם XML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/working-with-xml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
