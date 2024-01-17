---
title:                "Json के साथ काम करना"
html_title:           "Bash: Json के साथ काम करना"
simple_title:         "Json के साथ काम करना"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/working-with-json.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

JSON काम करने का एक तरीका है जो की डेटा को बेहतर ढंग से ओरगनाइज़ करे। बहुत से प्रोग्रामर्स अपने काम को एकदम सरल और आसान बनाने के लिए JSON का इस्तेमाल करते हैं।

## कैसे करें?

```Bash
# साधारण JSON फ़ाइल
{
  "नाम": "अजय",
  "उम्र": 30,
  "शहर": "दिल्ली"
}

# JSON को पढ़ने के लिए
cat file.json

# स्ट्रिंग तरीके से डेटा को ओरगनाइज़ करने के लिए
NAME="अजय"
AGE=30
CITY="दिल्ली"
```
```Bash
# अन्य कमांड्स जो फ़ाइल में JSON डेटा वापस करते हैं
cat file.json | grep "नाम"
cat file.json | jq .उम्र
```

```Bash
# डेटा डालना
echo '{"नाम": "अजय", "उम्र": 30, "शहर": "दिल्ली"}' >> file.json
```

## गहराई में जाएं

JSON स्थापित होने से पहले, XML से डेटा ओरगनाइज़ करने के लिए उपयोग किया जाता था। अपने सरल स्ट्रक्चर का कारण, बहुत से सोशल मीडिया साईट और API अब भी JSON का इस्तेमाल करते हैं। अगर आप अपने काम को और भी आसान बनाना चाहते हैं, तो आप JSON के बारे में अधिक सीख सकते हैं।

## सहायता मिलेगी

- [JSON Wikipedia पेज](https://en.wikipedia.org/wiki/JSON)
- [Bash JSON स्क्रिप्टिंग उदाहरण](https://devhints.io/bash-json)
- [Bash JSON मैन्युअल](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Shell-Parameter-Expansion)