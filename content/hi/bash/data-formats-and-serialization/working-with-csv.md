---
title:                "CSV के साथ काम करना"
aliases:
- /hi/bash/working-with-csv.md
date:                  2024-02-03T19:19:14.744980-07:00
model:                 gpt-4-0125-preview
simple_title:         "CSV के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
CSV (Comma-Separated Values) फाइलों के साथ Bash में काम करना प्लेन टेक्स्ट फॉर्मेट में संग्रहीत टेबुलर डेटा को संसाधित और हेरफेर करने के बारे में है। यह प्रोग्रामरों के लिए अत्यावश्यक है क्योंकि इससे कमांड लाइन से सीधे डेटा परिवर्तन, विश्लेषण, और एकीकरण के कार्यों का स्वचालन संभव होता है, बिना अधिक भारी-भरकम उपकरणों या प्रोग्रामिंग वातावरणों की आवश्यकता के।

## कैसे करें:

**एक CSV फाइल को लाइन दर लाइन पढ़ना**

```bash
while IFS=, read -r column1 column2 column3
do
  echo "कॉलम 1: $column1, कॉलम 2: $column2, कॉलम 3: $column3"
done < sample.csv
```

*नमूना उत्पादन:*

```
कॉलम 1: id, कॉलम 2: name, कॉलम 3: email
...
```

**किसी शर्त के आधार पर CSV पंक्तियों को फ़िल्टर करना**

`awk` का उपयोग करते हुए, आप आसानी से पंक्तियों को फ़िल्टर कर सकते हैं। उदाहरण के लिए, जहां दूसरा कॉलम "Alice" के बराबर है, वहाँ के पंक्तियों को ढूँढना:

```bash
awk -F, '$2 == "Alice" { print $0 }' sample.csv
```

**एक कॉलम मान को संशोधित करना**

दूसरे कॉलम को अपरकेस में बदलने के लिए:

```bash
awk -F, 'BEGIN {OFS=",";} { $2 = toupper($2); print $0; }' sample.csv
```

**एक कॉलम के आधार पर एक CSV फाइल को क्रमबद्ध करना**

आप एक CSV फाइल को क्रमबद्ध कर सकते हैं, मान लीजिए, तीसरे कॉलम के आधार पर (संख्यात्मक रूप से):

```bash
sort -t, -k3,3n sample.csv
```

**अधिक जटिल कार्यों के लिए `csvkit` का उपयोग करना**

`csvkit` CSV के साथ काम करने और इसमें बदलने के लिए कमांड-लाइन टूल्स का एक सूट है। इसे pip के माध्यम से इंस्टॉल किया जा सकता है।

एक JSON फाइल को CSV में बदलने के लिए:

```bash
in2csv data.json > data.csv
```

SQL का उपयोग करके एक CSV फाइल को क्वेरी करना:

```bash
csvsql --query "SELECT name FROM sample WHERE id = 10" sample.csv
```

*नोट: `csvkit` को इंस्टॉल करने की आवश्यकता होती है Python और इसे `pip install csvkit` का उपयोग करके किया जा सकता है।*
