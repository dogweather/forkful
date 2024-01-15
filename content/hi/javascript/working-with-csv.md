---
title:                "कंप्यूटर प्रोग्रामिंग में सीएसवी के साथ काम करना"
html_title:           "Javascript: कंप्यूटर प्रोग्रामिंग में सीएसवी के साथ काम करना"
simple_title:         "कंप्यूटर प्रोग्रामिंग में सीएसवी के साथ काम करना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

## क्यों

CSV (Comma Separated Values) फाइलों को प्रोग्रामिंग के माध्यम से पढ़ने और उनसे काम करना बहुत सारे उपयोगों के लिए उपयोगी है। यह डाटा संरचना को अधिक स्पष्ट और साझा करता है जो अपनी आसानी से चलने वाले फॉर्मेट के लिए जाना जाता है।

## कैसे करें

```Javascript
// CSV फ़ाइल को पढ़ें
const csv = require('csv-parser');
const fs = require('fs');

fs.createReadStream('data.csv')
  .pipe(csv())
  .on('data', (row) => {
    console.log(row);
  })
  .on('end', () => {
    console.log('CSV फ़ाइल से डेटा पढ़ना सम्पन्न हुआ।');
  });

// CSV फ़ाइल लिखें
const createCsvWriter = require('csv-writer').createObjectCsvWriter;
const csvWriter = createCsvWriter({
  path: 'output.csv',
  header: [
    { id: 'name', title: 'नाम' },
    { id: 'age', title: 'आयु' },
    { id: 'job', title: 'नौकरी' },
  ]
});

const data = [
  {
    name: 'जॉन डो',
    age: 30,
    job: 'मैनेजर'
  },
  {
    name: 'सारा सिंह',
    age: 25,
    job: 'डिज़ाइनर'
  },
  {
    name: 'राहुल मेहता',
    age: 35,
    job: 'खाता अधिकारी'
  }
];

csvWriter.writeRecords(data)
  .then(() => {
    console.log('CSV फ़ाइल बनाना सम्पन्न हुआ।');
  });

```

Output:
```
{ name: 'जॉन डो', age: 30, job: 'मैनेजर' }
{ name: 'सारा सिंह', age: 25, job: 'डिज़ाइनर' }
{ name: 'राहुल मेहता', age: 35, job: 'खाता अधिकारी' }
CSV फ़ाइल से डेटा पढ़ना सम्पन्न हुआ।
CSV फ़ाइल बनाना सम्पन्न हुआ।
```

## डीप डाइव

CSV फाइलें विशेष रूप से डाटा सिंक्रनाइज़ेशन और संचयन के लिए बहुत उपयोगी हैं। यह डेटा मानचित्रण और अनुकूलन के लिए अनुकूल होती है जो विभिन्न एप्लिकेशन और प्लेटफॉर्म में संभावित होता है। इसलिए, जब