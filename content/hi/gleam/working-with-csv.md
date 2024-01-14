---
title:                "Gleam: सीएसवी के साथ काम करना"
simple_title:         "सीएसवी के साथ काम करना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

## क्यों
क्योंकि CSV फ़ॉर्मैट संरचित डेटा को स्प्रेडशीट और डेटा बेस के रूप में स्टोर करता है, जिससे उसे आसानी से पढ़ा और बदला जा सकता है। इसलिए, CSV डेटा को व्यवस्थित रूप से काम करने के लिए, लोगों को उसके साथ काम करना सीखना बहुत महत्वपूर्ण है।

## कैसे करें
```Gleam  
import gleam/csv

csv_data = '''
Name, Age, City
John, 30, New York
Jane, 25, London
'''

parsed_data = csv.parse(csv_data)

for row in parsed_data do
  name = row["Name"]
  age = row["Age"]
  city = row["City"]
  csv.print("Name: " ++ name ++ " | Age: " ++ age ++ " | City: " ++ city)
end
```
```
Name: John | Age: 30 | City: New York
Name: Jane | Age: 25 | City: London
```

## गहराई में दूबना
CSV के साथ काम करना अधिक दूर तक बढ़ावा देने के लिए, आप उसे उपयोग करने के साथ से उसे लगातार आगे बढ़ा सकते हैं। आप उसमें फिल्टरिंग, सहभागिकरण, और साथ ही CSV उपयोग करके बहुत कुछ कर सकते हैं।

## अधिक जानकारी के लिए
[ग्लीम डॉक्यूमेंटेशन]("https://gleam.run/documentation/")
[CSV मोड्यूल का स्रोत कोड]("https://github.com/gleam-lang/gleam/blob/master/lib/csv/csv.gleam")
[CSV फॉर्मैट के बारे में अधिक जानकारी]("https://en.wikipedia.org/wiki/Comma-separated_values")