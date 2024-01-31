---
title:                "CSV के साथ काम करना"
date:                  2024-01-19
html_title:           "Bash: CSV के साथ काम करना"
simple_title:         "CSV के साथ काम करना"

category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
CSV (Comma Separated Values) एक साधारण फाइल फॉर्मेट है जिसमें डेटा अल्पविराम से अलग होता है। प्रोग्रामर्स को डेटा आयात करने, संग्रहित करने और विश्लेषण करने के लिए CSV से काम लेना पड़ता है क्योंकि यह संरचना सरल और व्यापक रूप से स्वीकृत है।

## How to: (कैसे करें:)
यहाँ एक साधारन Go कोड है जो CSV फाइल से डेटा पढ़ता है:

```Go
package main

import (
    "encoding/csv"
    "fmt"
    "os"
)

func main() {
    file, err := os.Open("example.csv")
    if err != nil {
        panic(err)
    }
    defer file.Close()

    reader := csv.NewReader(file)
    records, err := reader.ReadAll()
    if err != nil {
        panic(err)
    }

    for _, record := range records {
        fmt.Println(record)
    }
}
```

अगर `example.csv` में डेटा यह हो:
```
name,age,city
John,30,New York
Alice,25,Sydney
```

तो आउटपुट होगा:
```
[name age city]
[John 30 New York]
[Alice 25 Sydney]
```

## Deep Dive (गहराई में जानकारी)
CSV फॉर्मेट का इतिहास 1970 के दशक में शुरू हुआ था और इसने तब से डेटा इंटरचेंज के एक मानक रूप के तौर पर अपनी जगह बनाई है। इसके विकल्पों में JSON, XML, और YAML शामिल हैं - लेकिन CSV इसकी सादगी के कारण लगातार लोकप्रिय है। Go में CSV फाइलों को संभालने के लिए `encoding/csv` पैकेज मौजूद है, जो विभिन्न प्रकार के ऑपरेशन्स जैसे कि पढ़ना और लिखना आसान बनाता है।

## See Also (अन्य सूत्रों)
- Go के अधिकृत CSV पैकेज का डॉक्यूमेंटेशन: https://pkg.go.dev/encoding/csv
- CSV फाइल फॉर्मेट के बारे में विकिपीडिया पेज: https://en.wikipedia.org/wiki/Comma-separated_values
- JSON और XML की तुलना में CSV के उपयोग: https://www.data.gov/developers/blog/comparison-of-csv-json-and-xml-formats
