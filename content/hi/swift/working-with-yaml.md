---
title:                "yaml के साथ काम करना"
html_title:           "Swift: yaml के साथ काम करना"
simple_title:         "yaml के साथ काम करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
YAML काम करना और समझने के लिए सरल है, और इसलिए कई प्रोग्रामर इस फाइल फॉर्मेट का उपयोग करते हैं। YAML एक अधिक संरचनात्मक फाइल प्रारूप है जो डेटा को अधिक अनुकूलित और शानदार ढंग से प्रदर्शित करने में मदद करता है।

## कैसे और क्यों?
आप अपनी Swift एप्लिकेशन में YAML फ़ाइलें को कैसे सक्रिय कर सकते हैं, इसके लिए निम्नलिखित कोड ब्लॉक का उपयोग करें:

```Swift
let yamlString = """
key1: value1
key2: [value2a, value2b, value2c]
key3:
    - subkey3a: subvalue3a
    - subkey3b: subvalue3b
"""
// एकसाथ तीन तरह के वैल्यू को एक समूह में असाइन करने के लिए उपयोग करें

let yamlDict = try! YAMLDecoder().decode([String: Any].self, from: yamlString)

print(yamlDict["key1"]) // इस तरह से आप वैल्यू को प्रिंट कर सकते हैं।
```

आप अपनी YAML फ़ाइलों में प्रारूपन और संरचना के लिए भी उपयोग कर सकते हैं। YAML फ़ाइलों में टैब का उपयोग संख्याओं को अलग करने के लिए उपयोगी हो सकता है, जिससे आपकी फ़ाइल अधिक स्पष्ट हो।

## गहराई में
YAML का अर्थ है "YAML अद्यतन मार्कअप भाषा"। YAML एक SML का एक प्रारूप है जो कि सरलता और पारगमन को बढ़ावा देता है। यह भाषा एक और विस्तृत संरचना है जो मानक के रूप में अनुमोदित है।

आप YAML को एक फोर्मैट के रूप में जान सकते हैं, जिससे आप न केवल डेटा को संरचित कर सकते हैं, बल्कि साथ ही साथ अन्य फ़ाइलों को भी YAML में बदल सकते हैं। YAML का उपयोग करने के लिए आपको पहले YAML मानक को समझना होगा।

## देखें भी
- [YAML मानक दस्तावेज़ीकरण](https://yaml.org/)
- [YAML गाइड](https://docs.ansible.com/ansible/latest/reference_appendices/YAMLSyntax.html)
- [YAML online parser](https://yaml-online-parser.appspot.com/)