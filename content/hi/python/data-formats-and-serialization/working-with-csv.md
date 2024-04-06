---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:32.196382-07:00
description: "\u0915\u0948\u0938\u0947: \u092A\u093E\u092F\u0925\u0928 \u092E\u0947\
  \u0902 \u092C\u093F\u0932\u094D\u091F-\u0907\u0928 `csv` \u092E\u0949\u0921\u094D\
  \u092F\u0942\u0932 CSV \u092B\u093E\u0907\u0932\u094B\u0902 \u0915\u094B \u0938\u0902\
  \u092D\u093E\u0932\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u092A\u094D\u0930\
  \u0926\u093E\u0928 \u0915\u093F\u092F\u093E \u0917\u092F\u093E \u0939\u0948, \u091C\
  \u093F\u0938\u0938\u0947 \u0907\u0928\u092E\u0947\u0902 \u092A\u0922\u093C\u0928\
  \u093E \u0914\u0930 \u0932\u093F\u0916\u0928\u093E \u0938\u0940\u0927\u093E \u0939\
  \u094B \u091C\u093E\u0924\u093E \u0939\u0948\u0964 \u0905\u0927\u093F\u0915 \u092E\
  \u091C\u092C\u0942\u0924 \u0914\u0930 \u091C\u091F\u093F\u0932\u2026"
lastmod: '2024-04-05T21:53:53.648132-06:00'
model: gpt-4-0125-preview
summary: "\u092A\u093E\u092F\u0925\u0928 \u092E\u0947\u0902 \u092C\u093F\u0932\u094D\
  \u091F-\u0907\u0928 `csv` \u092E\u0949\u0921\u094D\u092F\u0942\u0932 CSV \u092B\u093E\
  \u0907\u0932\u094B\u0902 \u0915\u094B \u0938\u0902\u092D\u093E\u0932\u0928\u0947\
  \ \u0915\u0947 \u0932\u093F\u090F \u092A\u094D\u0930\u0926\u093E\u0928 \u0915\u093F\
  \u092F\u093E \u0917\u092F\u093E \u0939\u0948, \u091C\u093F\u0938\u0938\u0947 \u0907\
  \u0928\u092E\u0947\u0902 \u092A\u0922\u093C\u0928\u093E \u0914\u0930 \u0932\u093F\
  \u0916\u0928\u093E \u0938\u0940\u0927\u093E \u0939\u094B \u091C\u093E\u0924\u093E\
  \ \u0939\u0948\u0964 \u0905\u0927\u093F\u0915 \u092E\u091C\u092C\u0942\u0924 \u0914\
  \u0930 \u091C\u091F\u093F\u0932 \u0921\u0947\u091F\u093E \u092E\u0947\u0928\u093F\
  \u092A\u0941\u0932\u0947\u0936\u0928 \u0915\u0947 \u0932\u093F\u090F, \u0925\u0930\
  \u094D\u0921-\u092A\u093E\u0930\u094D\u091F\u0940 \u0932\u093E\u0907\u092C\u094D\
  \u0930\u0947\u0930\u0940 `pandas` \u092C\u0939\u0941\u0924 \u0932\u094B\u0915\u092A\
  \u094D\u0930\u093F\u092F \u0939\u0948\u0964."
title: "CSV \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E"
weight: 37
---

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
