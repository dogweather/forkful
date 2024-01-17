---
title:                "csv के साथ काम करना"
html_title:           "TypeScript: csv के साथ काम करना"
simple_title:         "csv के साथ काम करना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
CSV (Comma Separated Values) संरचना एक डेटा को संरचित रूप में प्रदर्शित करने के लिए एक सरल तरीका है। प्रोग्रामर्स अक्सर इसका उपयोग डेटा को भी ढांचा देने या इसको एक से दूसरे प्रारूप में बदलने के लिए करते हैं।

## कैसे करें:
```TypeScript
import * as fs from 'fs';
import * as csv from 'csv-parser';

fs.createReadStream('data.csv')
    .pipe(csv({
        separator: ','
    }))
    .on('data', (data) => {
        // डेटा प्रिंट करें
        console.log(data);
    })
    .on('end', () => {
        console.log('CSV फाइल से डेटा सफलतापूर्वक प्राप्त हुआ');
    });
```

यहाँ, हम `data.csv` फाइल से डेटा पढ़ने के लिए `csv-parser` पैकेज का उपयोग करते हैं और डेटा को अलग-अलग स्ट्रिंग में बांटते हैं जोकि ',' के आधार पर अलग होते हैं। इसके बाद, हम `console.log()` फ़ंक्शन का उपयोग करके डेटा की जाँच करते हैं।

## गहराई में जाएं:
CSV फाइलें 1970 के दशक में पहली बार अपनी उपस्थिति को दर्शाई गई थीं। लोकप्रियता के कारण, आज भी अनेक प्रोग्रामिंग भाषाओं जैसे कि JavaScript, Python और Java में CSV समर्थित है। CSV की अवधारणा एक सरल रूप से स्प्रेडशीट की शीटों को CSV फाइलों में ठीक से संरचित करने के लिए डिज़ाइन की गई थी।

अन्य विकल्प की तुलना में, यह स्प्रेडशीट डेटा को संरचित करने के लिए बहुत ही सरल तरीका है। दूसरे फॉर्मेट जैसे JSON या XML को डेटा को बांटने और पढ़ने के लिए ज्यादा श्रमिक हो सकते हैं।

CSV कोन्वर्टर लाइब्रेरीज़ उपलब्ध हैं जो आपको फाइलों को अन्य प्रारूपों में बदलने में मदद कर सकते हैं। इसके अलावा, आप CSV फाइल में अतिरिक्त फील्ड जोड़ने या उन्हें संपादित करने के लिए आसानी से डेटा को पढ़ने और लिखने का अनुमति देता है।

## संबंधित स्रोत देखें:
- [CSV फाइल फॉर्मेट](https://en.wikipedia.org/wiki/Comma-separated_values)
- [csv-parser पैकेज](https://www.npmjs.com/package/csv-parser)
- [CSV कोन्वर्टर लाइब्रेरीज़](https://www.npmjs.com/package/csvtojson)