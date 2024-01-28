---
title:                "TOML के साथ काम करना"
date:                  2024-01-26T04:24:44.899165-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOML के साथ काम करना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/working-with-toml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
TOML के साथ काम करना का मतलब है TOML (Tom's Obvious, Minimal Language) फाइलों को कोड के साथ पार्सिंग और जनरेट करना। प्रोग्रामर इसे आसानी से पढ़ने योग्य कॉन्फ़िग फाइलों और डेटा सीरियलाइजेशन के लिए इस्तेमाल करते हैं, इसके स्पष्ट संकेतन और पारंपरिक डेटा प्रकारों के साथ संगतता के लिए धन्यवाद।

## कैसे:
Gleam में बिल्ट-इन TOML सपोर्ट नहीं है, इसलिए आपको एक बाहरी लाइब्रेरी की आवश्यकता होगी। उदाहरण के लिए:

```gleam
// मान लिजिये आपके पास एक TOML पार्सिंग लाइब्रेरी है:
import toml/{Parser, Encoder}

// TOML सामग्री पार्स करें
let toml_content = """
[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
"""

let parsed = Parser.parse(toml_content)

// पार्स किए गए डेटा का उपयोग करें
match parsed {
  Ok(data) -> "डेटा सफलतापूर्वक पार्स किया गया!"
  Error(_) -> "डेटा पार्स करने में विफल।"
}

// Gleam डेटा संरचना से TOML सामग्री जनरेट करें
let data = #{
  "owner": #{
    "name": "Tom Preston-Werner",
    "dob": "1979-05-27T07:32:00Z"
  }
}

let toml_string = Encoder.encode(data)
```

नमूना आउटपुट:

```
डेटा सफलतापूर्वक पार्स किया गया!
```

## गहराई से विचार
TOML को 2013 में Tom Preston-Werner ने रिलीज़ किया था। इसका उद्देश्य: XML की तुलना में अधिक पठनीय और सरल होना और YAML की तुलना में कम जटिल होना फाइल कॉन्फ़िगरेशनों के लिए। सादगी के बावजूद, यह संरचित डेटा के लिए रोबस्ट है, स्पष्ट और आसानी से समझने वाले सिंटेक्स की पेशकश करता है। वैकल्पिक में JSON, YAML, और INI शामिल हैं, लेकिन टॉमल की मिनिमलिस्टिक और स्पष्ट सिंटेक्स अक्सर कॉन्फ़िग फाइलों के लिए बाहर निकलती है। Gleam में TOML को लागू करना में दो मुख्य क्रियाएँ शामिल हैं: TOML को मूल डेटा संरचनाओं में पार्सिंग करना और मूल डेटा संरचनाओं को TOML में सीरियलाइज़ करना। BEAM भाषाओं के साथ इसकी संगतता के कारण, Gleam में अधिकांश TOML लाइब्रेरियाँ Erlang या Elixir के लिए उपयोग की जा सकती हैं, जिससे Gleam प्रोजेक्ट्स में सीमलेस एकीकरण सुनिश्चित होता है।

## देखें भी
- TOML भाषा स्पेक्स: [https://toml.io/en/](https://toml.io/en/)
- एक Erlang TOML पार्सर: [https://hex.pm/packages/toml](https://hex.pm/packages/toml)
- TOML GitHub पर: [https://github.com/toml-lang/toml](https://github.com/toml-lang/toml)
