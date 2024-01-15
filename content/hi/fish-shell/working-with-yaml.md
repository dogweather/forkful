---
title:                "यामल के साथ काम करना"
html_title:           "Fish Shell: यामल के साथ काम करना"
simple_title:         "यामल के साथ काम करना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## क्यों

यदि आप एक डेवलपर हों और फिश शेल को अपने प्रोग्रामिंग कामों में उपयोग करते हों, तो आपको कभी-कभी YAML फाइलों के साथ काम करने की जरूरत हो सकती है। YAML एक स्ट्रक्चर्ड टेक्स्ट फॉर्मेट है जिसका उपयोग कॉंफिगरेशन फाइलों बनाने और पैरसिस से करने में किया जाता है। आइए जानते हैं कि आप फिश शेल के साथ YAML को कैसे हैंडल कर सकते हैं।

## कैसे करें

```Fish Shell
# एक YAML फाइल बनाएं
touch config.yaml

# फाइल में कुछ सैम्पल कॉंफिगरेशन डेटा जोड़ें
echo 'name: John Doe
age: 25
hobbies:
- reading
- coding' > config.yaml

# यामल फाइल को पारस के माध्यम से पढ़ें
set -x data (parse_yaml config.yaml)
echo $data[name]  # आउटपुट: John Doe
echo $data[age]   # आउटपुट: 25
echo $data[hobbies]  # आउटपुट: "reading coding"
```

ऊपर दिए गए कोड स्निपेट्स में हमने पहले एक YAML फाइल बनाई है और उसमें कुछ सैम्पल कॉंफिगरेशन डेटा डाला है। उसके बाद हमने फाइल को पारस के माध्यम से पढ़ा और डेटा को उसके अलग-अलग कीवर्ड्स में एक्सेस किया है। इस तरह से आप फिश शेल में यामल को बहुत ही आसानी से बना सकते हैं और अपने कामों को और भी सुविधाजनक बना सकते हैं।

## गहराई में जानें

पहले भी बताया गया है कि YAML एक स्ट्रक्चर्ड टेक्स्ट फॉर्मेट है, लेकिन उसके गुणत्व और गहराई को समझने के लिए अधिक पढ़ें। YAML में कुछ अन्य डेटा टाइप्स भी हैं जो समर्थित हैं