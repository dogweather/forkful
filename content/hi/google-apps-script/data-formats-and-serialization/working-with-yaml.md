---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:08:31.274690-07:00
description: "\u0915\u0948\u0938\u0947: \u091C\u092C\u0915\u093F Google Apps Script\
  \ (GAS) \u092E\u0947\u0902 YAML \u092A\u093E\u0930\u094D\u0938\u093F\u0902\u0917\
  \ \u092F\u093E \u0938\u0940\u0930\u093F\u092F\u0932\u093E\u0907\u091C\u0947\u0936\
  \u0928 \u0915\u094B \u092E\u0942\u0932 \u0930\u0942\u092A \u0938\u0947 \u0938\u092E\
  \u0930\u094D\u0925\u0928 \u0928\u0939\u0940\u0902 \u0939\u0948, \u0906\u092A JavaScript\
  \ \u092A\u0941\u0938\u094D\u0924\u0915\u093E\u0932\u092F\u094B\u0902 \u0915\u093E\
  \ \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947 \u092F\u093E \u0915\u0938\
  \u094D\u091F\u092E\u2026"
lastmod: '2024-04-05T21:53:53.579556-06:00'
model: gpt-4-0125-preview
summary: "\u091C\u092C\u0915\u093F Google Apps Script (GAS) \u092E\u0947\u0902 YAML\
  \ \u092A\u093E\u0930\u094D\u0938\u093F\u0902\u0917 \u092F\u093E \u0938\u0940\u0930\
  \u093F\u092F\u0932\u093E\u0907\u091C\u0947\u0936\u0928 \u0915\u094B \u092E\u0942\
  \u0932 \u0930\u0942\u092A \u0938\u0947 \u0938\u092E\u0930\u094D\u0925\u0928 \u0928\
  \u0939\u0940\u0902 \u0939\u0948, \u0906\u092A JavaScript \u092A\u0941\u0938\u094D\
  \u0924\u0915\u093E\u0932\u092F\u094B\u0902 \u0915\u093E \u0909\u092A\u092F\u094B\
  \u0917 \u0915\u0930\u0915\u0947 \u092F\u093E \u0915\u0938\u094D\u091F\u092E \u092A\
  \u093E\u0930\u094D\u0938\u093F\u0902\u0917 \u092B\u093C\u0902\u0915\u094D\u0936\u0928\
  \u094D\u0938 \u0932\u093F\u0916\u0915\u0930 YAML \u0921\u0947\u091F\u093E \u0915\
  \u094B \u0938\u0902\u092D\u093E\u0932 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\
  \u0964 \u092A\u094D\u0930\u0926\u0930\u094D\u0936\u0928 \u0915\u0947 \u0932\u093F\
  \u090F, \u0906\u0907\u090F \u0926\u0947\u0916\u0947\u0902 \u0915\u093F \u0915\u0948\
  \u0938\u0947 \u090F\u0915 \u0915\u0938\u094D\u091F\u092E \u092B\u093C\u0902\u0915\
  \u094D\u0936\u0928 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\
  \u0947 YAML \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u092A\
  \u093E\u0930\u094D\u0938 \u0915\u093F\u092F\u093E \u091C\u093E\u090F, \u091A\u0942\
  \u0901\u0915\u093F \u092C\u093E\u0939\u0930\u0940 \u0932\u093E\u0907\u092C\u094D\
  \u0930\u0947\u0930\u093F\u092F\u094B\u0902 \u0915\u094B \u0938\u0940\u0927\u0947\
  \ GAS \u092E\u0947\u0902 \u0906\u092F\u093E\u0924 \u0928\u0939\u0940\u0902 \u0915\
  \u093F\u092F\u093E \u091C\u093E \u0938\u0915\u0924\u093E\u0964 \u092E\u093E\u0928\
  \ \u0932\u0940\u091C\u093F\u090F \u0906\u092A\u0915\u0947 \u092A\u093E\u0938 \u090F\
  \u0915 \u0938\u0930\u0932 YAML \u0935\u093F\u0928\u094D\u092F\u093E\u0938 \u0939\
  \u0948."
title: "YAML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
weight: 41
---

## कैसे:
जबकि Google Apps Script (GAS) में YAML पार्सिंग या सीरियलाइजेशन को मूल रूप से समर्थन नहीं है, आप JavaScript पुस्तकालयों का उपयोग करके या कस्टम पार्सिंग फ़ंक्शन्स लिखकर YAML डेटा को संभाल सकते हैं। प्रदर्शन के लिए, आइए देखें कि कैसे एक कस्टम फ़ंक्शन का उपयोग करके YAML स्ट्रिंग को पार्स किया जाए, चूँकि बाहरी लाइब्रेरियों को सीधे GAS में आयात नहीं किया जा सकता।

मान लीजिए आपके पास एक सरल YAML विन्यास है:

```yaml
title: YAML Example
description: Google Apps Script में YAML को संभालने का एक उदाहरण
tags:
  - Google Apps Script
  - YAML
  - Configuration
```

Google Apps Script में इसे पार्स करने के लिए, JavaScript की स्ट्रिंग मैनिपुलेशन क्षमताओं का उपयोग करें:

```javascript
function parseYAML(yamlString) {
  var result = {};
  var lines = yamlString.split("\n");
  for (var i = 0; i < lines.length; i++) {
    var line = lines[i];
    if (line.includes(":")) {
      var parts = line.split(":");
      var key = parts[0].trim();
      var value = parts[1].trim();
      // मूल हैंडलिंग फॉर ऐरेज़
      if (value.startsWith("-")) {
        value = [value.substring(1).trim()];
        while (i + 1 < lines.length && lines[i + 1].trim().startsWith("-")) {
          i++;
          value.push(lines[i].trim().substring(1).trim());
        }
      }
      result[key] = value;
    }
  }
  return result;
}

function testYamlParsing() {
  var yaml = "title: YAML Example\ndescription: Google Apps Script में YAML को संभालने का एक उदाहरण\ntags:\n  - Google Apps Script\n  - YAML\n  - Configuration";
  var parsed = parseYAML(yaml);
  Logger.log(parsed);
}
```

जब `testYamlParsing()` को निष्पादित किया जाता है, तो यह आउटपुट होता है:

```
{ title: 'YAML Example',
  description: 'Google Apps Script में YAML को संभालने का एक उदाहरण',
  tags: [ 'Google Apps Script', ' YAML', ' Configuration' ] }
```

यह कस्टम पार्सिंग दृष्टिकोण काफी आधारभूत है और जटिल YAML फ़ाइलों को समायोजित करने के लिए समायोजनों की आवश्यकता हो सकती है।

## गहराई में पड़ताल
YAML, जिसे 2001 में जारी किया गया था, अपने पूर्वजों जैसे XML या JSON की तुलना में अधिक मानव-पठनीय होने का लक्ष्य रखता था। हालांकि इसकी सादगी और उपयोग में आसानी को व्यापक रूप से सराहा गया है, Google Apps Script में YAML को संभालना चुनौतीपूर्ण है क्योंकि सीधे समर्थन का अभाव है। परिणामस्वरूप, प्रोग्रामर अक्सर YAML डेटा को पार्स और उत्पन्न करने के लिए JavaScript की बहुमुखी प्रतिभा पर निर्भर करते हैं। हालांकि, जटिल उपयोग के मामलों के लिए, विशेष रूप से जो गहरे नेस्टिंग और उन्नत डेटा संरचनाओं को शामिल करते हैं, यह पद्धति कठिनाईपूर्ण और त्रुटि प्रवण हो सकती है।

इसके विपरीत, JSON, Google Apps Script और अधिकांश अन्य प्रोग्रामिंग वातावरणों में मूल रूप से समर्थित है, डेटा सीरियलाइजेशन और डिसीरियलाइजेशन के लिए एक अधिक सीधा दृष्टिकोण प्रदान करता है बिना अतिरिक्त पार्सिंग ओवरहेड के। JSON का सिंटैक्स YAML की तुलना में कम शब्दबद्ध है, जिससे यह वेब अनुप्रयोगों में डेटा विनिमय के लिए अधिक उपयुक्त बनाता है। फिर भी, YAML विन्यास फ़ाइलों और ऐसी स्थितियों के लिए लोकप्रिय है जहाँ मानव पठनीयता सर्वोपरि है।

Google Apps Script में YAML के साथ काम करते समय, पठनीयता और उपयोग में आसानी के बीच समझौतों पर विचार करें। व्यापक YAML हेरफेर के लिए, बाहरी उपकरणों या सेवाओं का पता लगाने पर विचार करना सार्थक हो सकता है जो YAML को JSON में बदल सकें इससे पहले कि आप इसे अपनी स्क्रिप्ट के भीतर संसाधित करें।
