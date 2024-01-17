---
title:                "जेसन के साथ काम करना"
html_title:           "C++: जेसन के साथ काम करना"
simple_title:         "जेसन के साथ काम करना"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/working-with-json.md"
---

{{< edit_this_page >}}

# क्या और क्यों?

JSON से काम करना क्या है और यह कैसे काम करता है? यह एक संरचना है जिससे प्रोग्रामर अपने डेटा को संरचित और समझने में सरल बना सकते हैं। यह एक साधन है जिससे डेटा को अन्य प्रोग्राम में भेजा जा सकता है और अन्य प्रोग्रामों से डेटा प्राप्त किया जा सकता है।

# कैसे करें:

```C++
#include <iostream>
#include <string>
#include <nlohmann/json.hpp> // आपको JSON लाइब्रेरीको अपने कोड में जोड़ना पड़ेगा
using json = nlohmann::json; // आप एलीयस का उपयोग कर केवल json नाम को उपयोग कर सकते हैं।

int main()
{
  // एक सरल JSON ओब्जेक्ट बनाएं
  json data = {
    {"name", "John"},
    {"age", 35},
    {"is_employee", true}
  };

  // ओब्जेक्ट को स्ट्रिंग में परिवर्तित करें
  std::string data_string = data.dump();

  // परिणाम देखें
  std::cout << data_string;

  return 0;
}

// प्रदर्शित परिणाम:
// {"name": "John", "age": 35, "is_employee": true}
```

# गहराई में जाएं:

(1) JSON का इतिहास और उसके पृष्ठभूमि के बारे में अधिक जानकारी के लिए, आप [Wikipedia](https://en.wikipedia.org/wiki/JSON) पर जाकर और जान सकते हैं। (2) आप डेटा को संरक्षित करने के लिए अन्य संरचनाओं के अलावा भी JSON का उपयोग कर सकते हैं, जैसे कि XML या YAML। (3) JSON को C++ में शामिल करने के लिए, आपको [nlohmann/json](https://github.com/nlohmann/json) लाइब्रेरी की आवश्यकता होगी और आप अपने कोड में उसे जोड़ सकते हैं।

# उपलब्ध स्रोत देखें:

- [JSON.org](https://json.org/): JSON के आधिकारिक वेबसाइट, जहां से आप JSON से संबंधित अधिक जानकारी प्राप्त कर सकते हैं।
- [C++ डॉक्यूमेंटेशन](https://github.com/nlohmann/json): nlohmann/json लाइब्रेरी के लिए C++ डॉक्यूमेंटेशन।
- [JSON वैधता जांच टूल](https://jsonlint.com/): ऑनलाइन JSON वैधता जांच टूल।