---
title:                "TOML के साथ काम करना"
date:                  2024-01-26T04:26:01.991638-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOML के साथ काम करना"

category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/working-with-toml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

TOML, जिसे Tom's Obvious, Minimal Language के लिए छोटा किया गया है, एक डेटा सीरियलाइजेशन प्रारूप है जिसे इसकी स्पष्ट सेमेंटिक्स के कारण पढ़ना आसान है। प्रोग्रामर इसे कॉन्फ़िगरेशन फाइलों के लिए उपयोग करते हैं, क्योंकि यह इंसान द्वारा पढ़े जाने योग्य और मशीन के लिए अनुकूल होने के बीच एक संतुलन बनाता है।

## कैसे:

PowerShell में, TOML को पार्स करने के लिए कोई मूल cmdlet नहीं है। आप आमतौर पर एक मॉड्यूल का उपयोग करेंगे या अगर आप PowerShell के साथ काम करना चाहते हैं तो TOML को JSON में बदलने के लिए `toml-to-json` जैसे उपकरण का उपयोग करेंगे। यहाँ `PowerShellTOML` नामक एक काल्पनिक मॉड्यूल के साथ कैसे करें, वह दिखाया गया है:

```PowerShell
# पहले, मॉड्यूल इंस्टॉल करें (कल्पनीय, प्रदर्शन के लिए)
Install-Module PowerShellTOML

# एक TOML फाइल आयात करें
$config = Import-TomlConfig -Path './config.toml'

# एक मूल्य पहुँचना
Write-Output $config.database.server

# 'config.toml' में नमूना TOML सामग्री:
# [database]
# server = "192.168.1.1"
# ports = [ 8001, 8001, 8002 ]
# connection_max = 5000

# नमूना आउटपुट:
# 192.168.1.1
```

## गहराई में

TOML का निर्माण टॉम प्रेस्टन-वर्नर, GitHub के सह-संस्थापक, ने कॉन्फ़िगरेशन फाइलों के लिए XML और YAML के एक सरल विकल्प के रूप में किया था। इसका पहला संस्करण 2013 में प्रकट हुआ था। TOML JSON के समकक्ष है लेकिन इसे अधिक इंसान-मित्रतापूर्ण बनाने के लिए डिजाइन किया गया है, जिससे यह लोगों द्वारा बनाए रखे जाने वाले कॉन्फ़िगरेशन के लिए एक अच्छा विकल्प है। विकल्पों में YAML, JSON, और XML शामिल हैं।

कार्यान्वयन के लिहाज से, एक PowerShell मॉड्यूल के लिए TOML सामान्यतः C# जैसी अधिक प्रदर्शन-उन्मुख भाषा में लिखे गए TOML लाइब्रेरी के आसपास एक रैपर होता है। PowerShell के पास TOML के लिए मूल समर्थन नहीं है, जिसके कारण ऐसे मॉड्यूल की आवश्यकता होती है जो TOML प्रारूप के साथ सुविधाजनक इंटरफ़ेस करने में सहायक हो।

## और देखें

- TOML मानक: https://toml.io/en/
- `toml` PowerShell मॉड्यूल के लिए GitHub रिपॉज़िटरी (पढ़ने के समय मौजूद है तो): https://github.com/powershell/PowerShellTOML
- TOML के लिए एक परिचय: https://github.com/toml-lang/toml
- डेटा सीरियलाइजेशन प्रारूपों की तुलना: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
