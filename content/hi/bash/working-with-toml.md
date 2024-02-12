---
title:                "TOML के साथ काम करना"
aliases:
- hi/bash/working-with-toml.md
date:                  2024-01-26T04:19:46.774834-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOML के साथ काम करना"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/working-with-toml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
TOML, जिसका पूरा नाम है Tom's Obvious, Minimal Language, एक डेटा सीरियलाइजेशन प्रारूप है। प्रोग्रामर्स इसे इसकी सादगी और पढ़ने में आसानी के कारण पसंद करते हैं; यह कॉन्फ़िग फ़ाइलों के लिए उत्तम है, YAML की तरह वाइब्स लेकिन इन्सानों के लिए JSON से कम झंझटी।

## कैसे करें:
सबसे पहले, बैश में TOML से खेलने के लिए `toml-cli` इंस्टॉल करें। यह TOML फाइलों को ऑन द फ्लाई पढ़ने या संपादित करने के लिए सहायक है।

```Bash
# toml-cli इंस्टॉल करें, हमारा छोटा सहायक TOML कार्यों के लिए
pip install toml-cli

# कल्पना करें आपके पास एक TOML फाइल है, 'config.toml'
echo -e 'title = "TOML Demo"\n\n[owner]\nname = "Tom"\ndob = 1979-05-27T07:32:00Z' > config.toml

# एक मूल्य पढ़ें
toml get config.toml owner.name
# आउटपुट: Tom

# एक मूल्य सेट करें
toml set config.toml 'owner.dob' '2000-01-01T00:00:00Z'
# विशेष सुझाव: कुँजी के लिए उद्धरण का उपयोग करें जिसमें बिंदु होते हैं या अजीब अक्षर!
```

## गहराई से जानकारी
इन्सानों के लिए JSON की बाधाओं की नापसंदगी से उत्पन्न TOML लगभग 2013 में आया। GitHub के सह-संस्थापक, टॉम प्रेस्टन-वर्नर, कुछ सुपर पठनीय चाहते थे। YAML और INI विकल्प थे लेकिन TOML दोनों का सर्वोत्तम है।

शीबैंग, आपके पास नेस्टेड डेटा और अर्रेज हैं, माइनस YAML के फुट गन्स और JSON के कर्ली ब्रेसेज़। TOML अब Rust के Cargo में कॉन्फ़िग के लिए एक जाना-माना नाम है, जो इसके डेवलपमेंट दुनिया में उदय को दर्शाता है। इसे एक स्पैक द्वारा संचालित किया जाता है, जिससे चीजें टाइट और अच्छी तरह से परिभाषित रहती हैं। आप लगभग किसी भी भाषा में पार्सर प्राप्त कर सकते हैं, जिससे यह व्यापक रूप से अपनाने योग्य है।

## देखें भी
- आधिकारिक TOML GitHub रेपो: https://github.com/toml-lang/toml
- PyPI पर toml-cli: https://pypi.org/project/toml-cli/
- डेटा-सीरियलाइजेशन प्रारूपों की तुलना: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
