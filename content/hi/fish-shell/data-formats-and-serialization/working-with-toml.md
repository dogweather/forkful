---
title:                "TOML के साथ काम करना"
aliases: - /hi/fish-shell/working-with-toml.md
date:                  2024-01-26T04:22:44.330032-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOML के साथ काम करना"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/working-with-toml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
TOML एक कॉन्फिग फ़ाइल प्रारूप है, जो इंसानों के लिए पढ़ने और लिखने में आसान है, और मशीनों के लिए पार्स और जनरेट करने में आसान है। प्रोग्रामर्स TOML के साथ काम करते हैं ताकि परियोजनाओं में पठनीयता कुंजी होने पर साफ, पदानुक्रमिक कॉन्फिग फ़ाइलों को स्पष्ट किया जा सके।

## कैसे करें:
Fish में TOML को पढ़ने और मैनिपुलेट करने के लिए, आप `yj` जैसे टूल का उपयोग कर सकते हैं, जो TOML को JSON में बदल सकता है। यहाँ कैसे है:

```fish
# Fisher के माध्यम से yj स्थापित करें
fisher install jorgebucaran/yj

# TOML को JSON में बदलें
echo 'title = "TOML उदाहरण"' | yj -tj

# नमूना आउटपुट
{"title":"TOML उदाहरण"}
```

TOML लिखने के लिए, आप प्रक्रिया को उलट देते हैं:

```fish
# JSON को TOML में परिवर्तित करें
echo '{"title":"JSON उदाहरण"}' | yj -jt

# नमूना आउटपुट
title = "JSON उदाहरण"
```

भारी उठाने के लिए, `toml-cli` जैसे समर्पित TOML CLI टूल पर विचार करें।

```fish
# toml-cli स्थापित करें
pip install toml-cli

# TOML फ़ाइल में एक मूल्य सेट करें
toml set pyproject.toml tool.poetry.version "1.1.4"

# TOML फ़ाइल से एक मूल्य प्राप्त करें
set version (toml get pyproject.toml tool.poetry.version)
echo $version
```

## गहराई में डाइव
TOML (Tom's Obvious, Minimal Language), जिसे 2013 में Tom Preston-Werner ने पेश किया था, INI के समान है लेकिन एक निर्धारित विशेषता और डेटा पदानुक्रम के साथ। JSON और YAML मुख्य विकल्प हैं, लेकिन उनके अपने व्यापार-बंद हैं: JSON इंसानों के लिए उतना अनुकूल नहीं है, जबकि YAML अधिक जटिल है। TOML का डिजाइन उन परिदृश्यों में फलता-फूलता है जहां कॉन्फिग फ़ाइलें अक्सर हाथ से बनाए रखी जाती हैं, सादगी और अभिव्यक्ति का संतुलन बनाए रखता है। जब इसके कार्यान्वयन की बात आती है, तो अधिकांश प्रोग्रामिंग भाषाओं के लिए TOML पार्सर उपलब्ध हैं, जिसमें Fish के लिए TomlBombadil भी शामिल है जो आपकी स्क्रिप्ट में सही तरीके से फिट बैठ सकता है।

## यह भी देखें
- TOML आधिकारिक विशिष्टता: https://toml.io
- `yj`, TOML, JSON, YAML, और XML के बीच परिवर्तित करने के लिए एक टूल: https://github.com/jorgebucaran/yj
- `toml-cli`, TOML के लिए एक कमांड-लाइन युटिलिटी: https://github.com/sdispater/toml-cli
