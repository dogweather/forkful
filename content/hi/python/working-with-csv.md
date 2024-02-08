---
title:                "CSV के साथ काम करना"
aliases:
- hi/python/working-with-csv.md
date:                  2024-02-03T19:21:32.196382-07:00
model:                 gpt-4-0125-preview
simple_title:         "CSV के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
CSV (Comma-Separated Values) के साथ काम करना इसमें से पढ़ना और CSV फाइलों में डेटा लिखना शामिल है, जो टैबुलर डेटा स्टोर करने के लिए एक सामान्य प्रारूप है। प्रोग्रामर इसे आसानी से डेटा आदान-प्रदान और संग्रहित करने के लिए करते हैं, एक सरल, पाठ-आधारित प्रारूप में जो विभिन्न प्लेटफॉर्मों और भाषाओं में व्यापक रूप से समर्थित है।

## कैसे:
पायथन में बिल्ट-इन `csv` मॉड्यूल CSV फाइलों को संभालने के लिए प्रदान किया गया है, जिससे इनमें पढ़ना और लिखना सीधा हो जाता है। अधिक मजबूत और जटिल डेटा मेनिपुलेशन के लिए, थर्ड-पार्टी लाइब्रेरी `pandas` बहुत लोकप्रिय है।

### `csv` मॉड्यूल का उपयोग करना

#### एक CSV फ़ाइल पढ़ना
```python
import csv

with open('sample.csv', mode='r') as file:
    csv_reader = csv.reader(file)
    for row in csv_reader:
        print(row)
```
*मान लें `sample.csv` में निम्नलिखित है:*
```
name,age,city
John,22,New York
Jane,28,Los Angeles
```
*आउटपुट:*
```
['name', 'age', 'city']
['John', '22', 'New York']
['Jane', '28', 'Los Angeles']
```

#### एक CSV फ़ाइल में लिखना
```python
import csv

rows = [['name', 'age', 'city'], ['Jack', '33', 'Chicago'], ['Emily', '41', 'Denver']]

with open('output.csv', mode='w', newline='') as file:
    writer = csv.writer(file)
    writer.writerows(rows)
```
*यह `output.csv` को बनाता है या इसे निम्नानुसार ओवरराइट करता है:*
```
name,age,city
Jack,33,Chicago
Emily,41,Denver
```

### CSV के लिए `pandas` का उपयोग करना
`pandas` एक शक्तिशाली लाइब्रेरी है जो डेटा मेनिपुलेशन को सरल बनाती है और CSV फाइलों सहित अन्य डेटा प्रारूपों के साथ काम करना आसान बनाती है।

#### पंडास इंस्टॉल करें
```shell
pip install pandas
```

#### पंडास के साथ एक CSV फ़ाइल पढ़ना
```python
import pandas as pd

df = pd.read_csv('sample.csv')
print(df)
```
*आउटपुट:*
```
    name  age         city
0   John   22    New York
1   Jane   28  Los Angeles
```

#### पंडास के साथ एक CSV फ़ाइल में लिखना
```python
import pandas as pd

df = pd.DataFrame({'name': ['Jack', 'Emily'], 'age': [33, 41], 'city': ['Chicago', 'Denver']})
df.to_csv('output_pandas.csv', index=False)
```
*यह `output_pandas.csv` को बनाता है या इसे निम्नानुसार ओवरराइट करता है:*
```
name,age,city
Jack,33,Chicago
Emily,41,Denver
```
