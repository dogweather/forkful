---
title:                "Computer Programming में csv के साथ काम करना"
html_title:           "Javascript: Computer Programming में csv के साथ काम करना"
simple_title:         "Computer Programming में csv के साथ काम करना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

# CSV क्या होता है और यह क्यों किया जाता है?

CSV से क्या समझा जाता है, यह हमें डेटा को अलग अलग टेक्स्ट के स्तर पर संगठित करने में मदद करता है। प्रोग्रामर्स CSV का प्रयोग अक्सर डेटा को इम्पोर्ट या एक्सपोर्ट करने के लिए करते हैं।

## कैसे करें:

```
const data = [
  ['उपयोगकर्ता नाम', 'ईमेल', 'आयु'],
  ['जॉन डो', 'johndoe@gmail.com', '25'],
  ['जेन डो', 'jendo@gmail.com', '21']
]

// डेटा को CSV फाइल में लिखें
const csv = data.map(row => row.join(',')).join('\n');
console.log(csv);

// एक CSV फ़ाइल से डेटा पढ़ें
const fs = require('fs').promises;
fs.readFile('data.csv', 'utf-8').then((data) => {
  data.split('\n').map(row => {
    console.log(row.split(','))
  });
});
```

उपरोक्त कोड से, हम डेटा सेट बनाते हैं और फिर उसे CSV फाइल में लिखते हैं। और उसी तरह से, हम CSV फाइल से डेटा पढ़ते हैं।

## गहराई में जाएं:

CSV या Comma Separated Values, एक विशेष तरह की फाइल है जहां डेटा अलग अलग कोटेशन से अलग होता है और कोटेशन से अलग अलग फ़ील्डों में अलग होता है। आजकल, CSV कोडिंग में व्यापक रूप से उपयोग किया जाता है और प्रोग्रामिंग के क्षेत्र में यह ज्यादातर डेटा को शेयर करने के लिए एक प्रमुख तरीका है।

## देखें:

- [CSV फ़ॉर्मेट का सारांश](https://tools.ietf.org/html/rfc4180)