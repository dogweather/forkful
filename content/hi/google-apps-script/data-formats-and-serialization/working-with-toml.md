---
title:                "TOML के साथ काम करना"
aliases: - /hi/google-apps-script/working-with-toml.md
date:                  2024-02-01T22:07:51.494166-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOML के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/google-apps-script/working-with-toml.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

TOML, जिसका मतलब है Tom's Obvious, Minimal Language, एक संरचना फाइल प्रारूप है जो इसके स्पष्ट सिमेंटिक्स के कारण पढ़ने में आसान है। प्रोग्रामर अक्सर इसे अप्लीकेशन में कॉन्फ़िगरेशन फाइलों के लिए उपयोग करते हैं क्योंकि यह सीधे और मानव-पठनीय है, जिससे विभिन्न वातावरणों में ऐप्लिकेशन सेटिंग्स और कॉन्फ़िगरेशन का प्रबंधन सहज हो जाता है।

## कैसे:

चूंकि Google Apps Script मूलतः JavaScript है जिसमें Google के ऐप्स सूट तक पहुँच है, Google Apps Script में सीधे TOML के साथ काम करना थोड़ी चतुराई चाहता है। Google Apps Script में TOML पार्सिंग का स्वाभाविक समर्थन नहीं है, लेकिन आप JavaScript लाइब्रेरियों का लाभ उठा सकते हैं या मूल आवश्यकताओं के लिए एक सरल पार्सर लिख सकते हैं।

उदाहरण के तौर पर एक सरल TOML कॉन्फ़िगरेशन स्ट्रिंग को पार्स करें:

```javascript
// TOML स्ट्रिंग
var tomlString = `
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
`;

// एक सरल TOML से JSON पार्सर फ़ंक्शन
function parseTOML(tomlStr) {
  var result = {};
  var currentSection = result;
  tomlStr.split(/\r?\n/).forEach(line => {
    line = line.trim();
    if (line.startsWith('[')) { // नया सेक्शन
      var sectionName = line.replace(/\[|\]/g, '');
      result[sectionName] = {};
      currentSection = result[sectionName];
    } else if (line) {
      var keyValue = line.split('=').map(part => part.trim());
      var key = keyValue[0];
      var value = eval(keyValue[1]); // सरलता के लिए eval का उपयोग; उत्पादन कोड में सावधानी बरतें
      currentSection[key] = value;
    }
  });
  return result;
}

// पार्सर का परीक्षण करें
var configObject = parseTOML(tomlString);
console.log(configObject);

```

`console.log` से मिलने वाला उदाहरण आउटपुट एक JSON ऑब्जेक्ट के समान होगा, जिससे Google Apps Script के भीतर कॉन्फ़िगरेशन प्रॉपर्टीज़ तक पहुँचना आसान हो जाएगा:

```json
{
  "database": {
    "server": "192.168.1.1",
    "ports": [8001, 8001, 8002],
    "connection_max": 5000,
    "enabled": true
  }
}
```

## गहरी डुबकी

TOML को Tom Preston-Werner, GitHub के संस्थापकों में से एक द्वारा बनाया गया था, कॉन्फ़िगरेशन फाइलों के लिए JSON से ज्यादा मानव-अनुकूल बनाने के लिए, साथ ही साथ स्पष्ट रूप से पार्स किए जाने की क्षमता को बनाए रखते हुए। यह जितना संभव हो उतना साधारण होने का लक्ष्य रखता है, एक लक्ष्य जो अपने कोडबेस में सादगी और पठनीयता प्राप्त करने के लिए प्रयासरत कई डेवलपमेंट प्रोजेक्ट्स की नैतिकता के साथ अच्छी तरह से मेल खाता है।

Google Apps Script के संदर्भ में, TOML का उपयोग कुछ अधिक व्ययिता पर्यावरण दे सकता है, दिए गए सीधे समर्थन की कमी और इसे मैन्युअल रूप से पार्स करने या तृतीय-पक्ष लाइब्रेरियों के माध्यम से पार्स करने की आवश्यकता के कारण। छोटे प्रोजेक्ट्स के लिए या जो Google के इकोसिस्टम में गहराई से एकीकृत नहीं हैं, वहां JSON या यहां तक कि स्क्रिप्ट प्रॉपर्टीज़ में सरल कुंजी-मूल्य पैर संरचनाएं पर्याप्त हो सकती हैं और और अधिक सीधे लागू करने में आसान हो सकती हैं। हालांकि, उन अनुप्रयोगों के लिए जो मानव-अनुकूल कॉन्फ़िगरेशन फाइलों को प्राथमिकता देते हैं और पहले से ही TOML के प्रति प्रतिबद्ध हैं, कस्टम स्क्रिप्ट्स के माध्यम से TOML पार्सिंग का एकीकरण बिना पसंदीदा कॉन्फ़िगरेशन पैराडाइम से भटके लचीलेपन और maintainability की एक उपयोगी परत जोड़ता है।
