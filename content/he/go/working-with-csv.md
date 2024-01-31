---
title:                "עבודה עם קבצי CSV"
date:                  2024-01-19
simple_title:         "עבודה עם קבצי CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)

CSV (Comma-Separated Values) משמש לאחסון והעברת נתונים טבולריים. מתכנתים משתמשים בו כי הוא פשוט, אוניברסלי וקל לקריאה ולכתיבה על-ידי תוכנות ובני אדם.

## How to (איך לעשות):

```Go
package main

import (
	"encoding/csv"
	"fmt"
	"os"
	"strings"
)

func main() {
	// יצירת נתוני CSV לדוגמה
	csvData := "שם,מספר\nרועי,1\nשירי,2"

	// קריאת נתונים ממחרוזת
	r := csv.NewReader(strings.NewReader(csvData))

	// קריאת כל השורות מה CSV
	records, err := r.ReadAll()
	if err != nil {
		fmt.Println("Error:", err)
		return
	}

	// הדפסת הדאטה
	for _, record := range records {
		fmt.Println(record)
	}

	// כתיבת נתוני CSV לקובץ
	recordsToWrite := [][]string{{"name", "number"}, {"Tomer", "3"}, {"Gal", "4"}}
	file, err := os.Create("output.csv")
	if err != nil {
		fmt.Println("Error:", err)
		return
	}
	defer file.Close()

	w := csv.NewWriter(file)
	w.WriteAll(recordsToWrite)
	if err := w.Error(); err != nil {
		fmt.Println("Error:", err)
	}

	fmt.Println("CSV written successfully.")
}
```

פלט:
```
[שם מספר]
[רועי 1]
[שירי 2]
CSV written successfully.
```

## Deep Dive (צלילה עמוקה):

CSV קיים מאז שנות ה-70 ונחשב כסטנדרט לשיתוף נתונים. חלופות כוללות JSON, XML ו-Excel, אך CSV עדיין נפוץ עקב פשטותו. בעבודה עם Go, מתכנתים ישתמשו בפקג' `encoding/csv` לקריאה וכתיבה, דרך `Reader` ו-`Writer`.

## See Also (ראו גם):

- התיעוד הרשמי של פקג' `csv` בGo: https://pkg.go.dev/encoding/csv
- מדריך Go לעבודה עם קבצי CSV: https://golang.org/doc/articles/encoding_csv.html
- על פורמט CSV בראשון: https://tools.ietf.org/html/rfc4180
