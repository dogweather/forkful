---
title:                "CSV के साथ काम करना"
html_title:           "TypeScript: CSV के साथ काम करना"
simple_title:         "CSV के साथ काम करना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## क्यों

CSV से काम करना ।

CSV (Comma Separated Values) एक फॉर्मैट है जिसमें डेटा को अलग-अलग स्तंभों और पंक्तियों के रूप में सादा और सुव्यवस्थित तरीके से स्टोर किया जाता है। यह फॉर्मैट बहुत संबंधपूर्ण होता है, क्योंकि इसका उपयोग डेटा को अन्य अनुप्रयोगों में अन्य फॉर्मैट में आसानी से पारित करने के लिए किया जाता है। इसलिए, इसका उपयोग किसी भी कारोबार या उद्योग में डेटा प्रसंस्करण करने के लिए किया जाता है।

## कैसे करें

टाइपस्क्रिप्ट में CSV से काम करना बहुत ही आसान है। सबसे पहले, ```csv-parse ``` और ```fs ``` जैसी पैकेज इंस्टॉल करने की जरूरत होती है। फिर नीचे दिए गए कोड ब्लॉक को अपने टाइपस्क्रिप्ट फाइल में लिखना होगा:

```
import * as csvParse from 'csv-parse';
import * as fs from 'fs';

const csvData: string = fs.readFileSync('data.csv', 'utf-8');

csvParse(csvData, { delimiter: ',' }, (err, data) => {
  if (err) {
    throw err;
  } else {
    console.log(data);
  }
});

```

इस कोड के माध्यम से, हम ```csvData ``` में CSV डेटा को पढ़ते हैं और उसे दिये गए डेलिमिटर के साथ अलग अलग स्तंभों और पंक्तियों में विभाजित करते हैं। फिर इसे कॉलबैक के माध्यम से डेटा को प्रिंट करते हैं। यदि कोई त्रुटि होती है, तो हम उसे कॉन्सोल पर प्रिंट कर देते हैं।

## गहराई में

CSV कोडिंग में कई मामलों में उपयोगी है, जैसे कि डेटा इंपोर्ट और एक्सपोर्ट, डाटा वैलिडेशन और एक्सेस जैस