---
title:                "עבודה עם XML"
date:                  2024-01-26T04:31:54.088126-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/working-with-xml.md"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם XML כוללת פרסור, יצירה והתעסקות עם מסמכי XML באמצעות קוד. מתכנתים עושים זאת לצורך החלפת נתונים, קבצי תצורה, ושירותי אינטרנט מכיוון שקריאות XML והתמיכה הרחבה בו הופכים אותו לבחירה מוצקה לנתונים מובנים.

## איך לעשות:
ב-Go, השתמשו בחבילה `encoding/xml`. בואו ננתח ונייצר XML.
```go
package main

import (
	"encoding/xml"
	"fmt"
	"os"
)

// מבנים מתואמים לאלמנטים של XML
type Plant struct {
	XMLName xml.Name `xml:"plant"`
	Id      int      `xml:"id,attr"`
	Name    string   `xml:"name"`
	Origin  []string `xml:"origin"`
}

func main() {
	coffee := &Plant{Id: 27, Name: "Coffee"}
	coffee.Origin = []string{"Ethiopia", "Brazil"}

	// ייצור מבנה ל-XML
	output, err := xml.MarshalIndent(coffee, " ", "  ")
	if err != nil {
		fmt.Printf("Error: %v\n", err)
	}

	os.Stdout.Write([]byte(xml.Header))
	os.Stdout.Write(output)

	// פרסור XML למבנה
	data := `
<plant id="27">
  <name>Coffee</name>
  <origin>Ethiopia</origin>
  <origin>Brazil</origin>
</plant>
`
	var p Plant
	if err := xml.Unmarshal([]byte(data), &p); err != nil {
		fmt.Printf("Error: %v", err)
		return
	}

	fmt.Printf("\n\nUnmarshaled: %+v", p)
}
```
דוגמת פלט:
```xml
<?xml version="1.0" encoding="UTF-8"?>
 <plant id="27">
   <name>Coffee</name>
   <origin>Ethiopia</origin>
   <origin>Brazil</origin>
 </plant>

Unmarshaled: {XMLName:{Space: Local:plant} Id:27 Name:Coffee Origin:[Ethiopia Brazil]}
```

## צלילה עמוקה
XML הוא קיים מאז סוף שנות ה-90, נוצר לפרסום אלקטרוני בקנה מידה גדול אך התקבל מהר לשימוש באינטרנט. אלטרנטיבות כמו JSON עלו לפופולריות משום שהן מופיעות כפשוטות יותר, אך הוולידציה של מסמכי XML דרך סכמות ומרחבי שמות נשארים חזקים למסמכים מורכבים. ב-Go, `encoding/xml` מטפל ברוב המשימות, אך למסמכים גדולים מאוד או עיבוד זרם, שקלו להשתמש ב-`xml.NewDecoder` ו-`xml.NewEncoder` לשליטה נמוכה יותר וביצועים טובים יותר.

## ראו גם
- חבילת `encoding/xml` של Go: https://pkg.go.dev/encoding/xml
- מדריך ל-XML: https://www.w3schools.com/xml/
- בלוג של Go על XML: https://blog.golang.org/xml
- השוואה בין JSON ל-XML: https://www.json.org/xml.html
