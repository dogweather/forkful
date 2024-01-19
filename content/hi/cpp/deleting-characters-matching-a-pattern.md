---
title:                "पैटर्न से मिलते जुलते वर्णों को हटाना"
html_title:           "Elixir: पैटर्न से मिलते जुलते वर्णों को हटाना"
simple_title:         "पैटर्न से मिलते जुलते वर्णों को हटाना"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# सी प्लस प्लस में पैटर्न मिलाने वाले वर्णों को हटाना
#### व्हाट और व्हाई?
पैटर्न मिलाने वाले वर्णों को हटाना का अर्थ होता है वचनों को मुक्त करना जो किसी निर्दिष्ट पैटर्न से मेल खाते हैं। प्रोग्रामर इसे तब करते हैं जब वे कोड को साफ और सटीक बनाने का प्रयास कर रहे होते हैं, या जब उन्हें उन्नत स्थर की त्रुटि खोजने की आवश्यकता होती है।

#### हाउ टू:
```C++
#include <algorithm>
#include <string>

int main()
{
	std::string str = "हेलो वर्ल्ड";
	str.erase(std::remove(str.begin(), str.end(), 'ल'), str.end());

	std::cout << str << std::endl;
	return 0;
}
```
उपरोक्त कोड का आउटपुट होगा:
```
"हेो वर्ड"
```
यहां हमने 'ल' वर्ण को हटाया है।

#### डीप डाइव:
सी प्लस प्लस में वर्णों को हटाना एसटीएल (स्टैंडर्द टेम्पलेट लाइब्रेरी) के कार्य का उपयोग करके किया जाता है। 'remove()' और 'erase()' कार्यों की मदद से हम वर्णों को हटा सकते हैं। 

'boost' एक और विकल्प है जिसका उपयोग आप वर्ण हटाने के लिए कर सकते हैं, लेकिन उससे कोड जटिल हो सकता है और न्याय समय में वृद्धि हो सकती है।

#### देखें भी:
1. सी प्लस प्लस रेफरेंस: http://www.cplusplus.com/reference/string/string/erase/
2. बूस्ट लाइब्रेरी: https://www.boost.org/doc/libs/1_73_0/libs/algorithm/doc/html/the_boost_algorithm_library.html