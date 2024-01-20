---
title:                "CSV के साथ काम करना"
html_title:           "Bash: CSV के साथ काम करना"
simple_title:         "CSV के साथ काम करना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
JavaScript में CSV का मतलब है 'Comma-Separated Values' - एक सरल फ़ाइल फॉर्मेट, जिसमें डाटा अल्पविराम से अलग होते हैं। प्रोग्रामर्स इसे इसलिए इस्तेमाल करते हैं क्योंकि यह स्प्रेडशीट और डेटाबेस की जानकारी को आसानी से स्टोर और शेयर करने का एक साधारण तरीका है।

## How to: (कैसे करें:)
```Javascript
// इस कोड के जरिए हम CSV स्ट्रिंग को पार्स करके जावास्क्रिप्ट ऑब्जेक्ट्स में बदलेंगे।

const csv = 'name,age\nAlice,23\nBob,34';

function csvToJSON(csv) {
  const lines = csv.split('\n');
  const headers = lines[0].split(',');
  return lines.slice(1).map(line => {
    const data = line.split(',');
    return headers.reduce((obj, nextKey, index) => {
      obj[nextKey] = data[index];
      return obj;
    }, {});
  });
};

console.log(csvToJSON(csv));
```

Sample Output:
```
[
  { name: 'Alice', age: '23' },
  { name: 'Bob', age: '34' }
]
```

## Deep Dive (गहराई से जानकारी):
CSV फॉर्मेट की खोज सन 1970s में हुई थी। ये JSON जैसे नए फॉर्मेट्स का विकल्प है लेकिन इसका सरल होना आज भी इसे फायदेमंद बनाता है। हालांकि, CSV में कॉम्प्लेक्स डाटा स्ट्रक्चर्स को स्टोर करना मुश्किल है। JavaScript में CSV का इस्तेमाल प्राय: फाइल्स को पार्स करने, API से डाटा फेच करने, या फाइल सिस्टम के ऑपरेशन्स के लिए होता है। 

## See Also (और भी जानकारी):
- [Papaparse](https://www.papaparse.com/): एक शक्तिशाली जावास्क्रिप्ट पार्सर के लिए वेबसाइट, जिससे बड़े CSV फाइल्स को पार्स करना आसान होता है।