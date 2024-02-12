---
title:                "YAML के साथ काम करना"
date:                  2024-02-03T19:25:46.474084-07:00
model:                 gpt-4-0125-preview
simple_title:         "YAML के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

YAML, जिसका पूरा नाम YAML Ain't Markup Language है, एक मानव-पठनीय डेटा सीरियलाइज़ेशन मानक है जिसे कॉन्फ़िगरेशन फ़ाइलों के लिए और उन अप्लिकेशन्स में डेटा संग्रहित या प्रेषित करने के लिए उपयोग किया जा सकता है जहां डेटा इस्तेमाल किया जा रहा है। प्रोग्रामर्स YAML की ओर इसकी स्पष्टता और सरलता के कारण आकर्षित होते हैं, विशेष रूप से उन प्रोजेक्ट्स में जिनमें जटिल कॉन्फ़िगरेशन शामिल हैं या आसानी से संपादन योग्य डेटा संरचनाएं आवश्यक होती हैं।

## कैसे:

Bash में सीधे YAML के साथ काम करना थोड़ी बुद्धिमत्ता की मांग करता है, क्योंकि Bash में YAML को पार्स करने के लिए निर्मित समर्थन नहीं है। हालाँकि, आप `yq` जैसे बाहरी उपकरण (एक हल्का और पोर्टेबल कमांड-लाइन YAML प्रोसेसर) का उपयोग करके YAML फाइलों के साथ कुशलतापूर्वक बातचीत कर सकते हैं। आइए कुछ सामान्य ऑपरेशनों के माध्यम से जाते हैं:

### `yq` की स्थापना:

उदाहरणों में जाने से पहले, सुनिश्चित करें कि आपके पास `yq` स्थापित है। आप इसे आमतौर पर अपने पैकेज मैनेजर से स्थापित कर सकते हैं, उदाहरण के लिए, उबंटू पर:

```bash
sudo apt-get install yq
```

या आप इसे सीधे इसके GitHub रिपॉजिटरी से डाउनलोड कर सकते हैं।

### मान पढ़ना:

मान लीजिए आपके पास `config.yaml` नामक एक फाइल है जिसमें निम्नलिखित सामग्री है:

```yaml
database:
  host: localhost
  port: 5432
user:
  name: admin
  password: secret
```

डाटाबेस होस्ट पढ़ने के लिए, आप `yq` का इस प्रकार उपयोग कर सकते हैं:

```bash
yq e '.database.host' config.yaml
```

**नमूना आउटपुट:**

```
localhost
```

### एक मान को अपडेट करना:

`config.yaml` में उपयोगकर्ता के नाम को अपडेट करने के लिए, `-i` (इन-प्लेस) विकल्प के साथ `yq eval` कमांड का उपयोग करें:

```bash
yq e '.user.name = "newadmin"' -i config.yaml
```

परिवर्तन की जाँच करें:

```bash
yq e '.user.name' config.yaml
```

**नमूना आउटपुट:**

```
newadmin
```

### एक नया तत्व जोड़ना:

डाटाबेस अनुभाग के अंतर्गत एक नया फील्ड `timeout` जोड़ने के लिए:

```bash
yq e '.database.timeout = 30' -i config.yaml
```

फाइल की सामग्री की जाँच पूर्ति की पुष्टि करेगी।

### एक तत्व को हटाना:

उपयोगकर्ता के पासवर्ड को हटाने के लिए:

```bash
yq e 'del(.user.password)' -i config.yaml
```

यह ऑपरेशन कॉन्फ़िगरेशन से पासवर्ड फ़ील्ड को हटा देगा।

याद रखें, `yq` एक शक्तिशाली उपकरण है और इसमें बहुत अधिक क्षमताएं हैं, जिसमें YAML से JSON में परिवर्तन, फ़ाइलों का मिलान, और यहां तक कि अधिक जटिल मैनिपुलेशन भी शामिल हैं। आगे की खोज के लिए `yq` दस्तावेज़ देखें।