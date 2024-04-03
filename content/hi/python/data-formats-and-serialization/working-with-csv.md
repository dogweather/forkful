---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:32.196382-07:00
description: "CSV (Comma-Separated Values) \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\
  \u092E \u0915\u0930\u0928\u093E \u0907\u0938\u092E\u0947\u0902 \u0938\u0947 \u092A\
  \u0922\u093C\u0928\u093E \u0914\u0930 CSV \u092B\u093E\u0907\u0932\u094B\u0902 \u092E\
  \u0947\u0902 \u0921\u0947\u091F\u093E \u0932\u093F\u0916\u0928\u093E \u0936\u093E\
  \u092E\u093F\u0932 \u0939\u0948, \u091C\u094B \u091F\u0948\u092C\u0941\u0932\u0930\
  \ \u0921\u0947\u091F\u093E \u0938\u094D\u091F\u094B\u0930 \u0915\u0930\u0928\u0947\
  \ \u0915\u0947 \u0932\u093F\u090F \u090F\u0915 \u0938\u093E\u092E\u093E\u0928\u094D\
  \u092F \u092A\u094D\u0930\u093E\u0930\u0942\u092A \u0939\u0948\u0964\u2026"
lastmod: '2024-03-13T22:44:51.629284-06:00'
model: gpt-4-0125-preview
summary: "CSV (Comma-Separated Values) \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\
  \u092E \u0915\u0930\u0928\u093E \u0907\u0938\u092E\u0947\u0902 \u0938\u0947 \u092A\
  \u0922\u093C\u0928\u093E \u0914\u0930 CSV \u092B\u093E\u0907\u0932\u094B\u0902 \u092E\
  \u0947\u0902 \u0921\u0947\u091F\u093E \u0932\u093F\u0916\u0928\u093E \u0936\u093E\
  \u092E\u093F\u0932 \u0939\u0948, \u091C\u094B \u091F\u0948\u092C\u0941\u0932\u0930\
  \ \u0921\u0947\u091F\u093E \u0938\u094D\u091F\u094B\u0930 \u0915\u0930\u0928\u0947\
  \ \u0915\u0947 \u0932\u093F\u090F \u090F\u0915 \u0938\u093E\u092E\u093E\u0928\u094D\
  \u092F \u092A\u094D\u0930\u093E\u0930\u0942\u092A \u0939\u0948\u0964 \u092A\u094D\
  \u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0907\u0938\u0947 \u0906\u0938\
  \u093E\u0928\u0940 \u0938\u0947 \u0921\u0947\u091F\u093E \u0906\u0926\u093E\u0928\
  -\u092A\u094D\u0930\u0926\u093E\u0928 \u0914\u0930 \u0938\u0902\u0917\u094D\u0930\
  \u0939\u093F\u0924 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0915\
  \u0930\u0924\u0947 \u0939\u0948\u0902, \u090F\u0915 \u0938\u0930\u0932, \u092A\u093E\
  \u0920-\u0906\u0927\u093E\u0930\u093F\u0924 \u092A\u094D\u0930\u093E\u0930\u0942\
  \u092A \u092E\u0947\u0902 \u091C\u094B \u0935\u093F\u092D\u093F\u0928\u094D\u0928\
  \ \u092A\u094D\u0932\u0947\u091F\u092B\u0949\u0930\u094D\u092E\u094B\u0902 \u0914\
  \u0930 \u092D\u093E\u0937\u093E\u0913\u0902 \u092E\u0947\u0902 \u0935\u094D\u092F\
  \u093E\u092A\u0915 \u0930\u0942\u092A \u0938\u0947 \u0938\u092E\u0930\u094D\u0925\
  \u093F\u0924 \u0939\u0948\u0964."
title: "CSV \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E"
weight: 37
---

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
