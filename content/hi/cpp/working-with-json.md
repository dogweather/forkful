---
title:                "C++: कंप्यूटर प्रोग्रामिंग में काम करना: जेसन के साथ"
simple_title:         "कंप्यूटर प्रोग्रामिंग में काम करना: जेसन के साथ"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/working-with-json.md"
---

{{< edit_this_page >}}

# क्यों

जेसन (JSON) एक लोकप्रिय डेटा interchange फॉर्मेट है जो विभिन्न प्रोग्रामिंग भाषाओं में उपयोग किया जाता है। यह डेटा को compaćt और readable भी बनाता है, जो इसे प्रोग्रामर्स के लिए एक आसान और पसंदीदा चयन बनाता है। इसलिए, जो लोग C++ भाषा में काम करते हैं, उन्हें जेसन को समझना और उसे कोड में इम्प्लीमेंट करना बहुत उपयोगी हो सकता है।

# कैसे

```C++
 // जेसन ऑब्जेक्ट बनाएं
 json j = {
    {"name", "John"},
    {"age", 25},
    {"hobbies", {"reading", "coding"}}
};

// जेसन ऑब्जेक्ट से डेटा अभिलेख निकालें
std::string name = j["name"];
int age = j["age"];
std::vector<std::string> hobbies = j["hobbies"];

// डेटा को जेसन ऑब्जेक्ट में जोड़ें
j["city"] = "Mumbai";

// जेसन ऑब्जेक्ट को स्ट्रिंग में रूपांतरित करें
std::string j_string = j.dump();

// स्ट्रिंग से जेसन ऑब्जेक्ट बनाएं
json new_j = json::parse(j_string);
```

#### आउटपुट:
```C++
Name: John
Age: 25
Hobbies:
1. Reading
2. Coding
City: Mumbai
```

# गहराई में

जेसन कोडिंग में थोड़ी सी depth जोड़ने के लिए, आप कुछ आगे गए तरीकों का भी इस्तेमाल कर सकते हैं:

- जेसन अंतरण (serialization) और अंशकरण (parsing) के लिए उपलब्ध लाइब्रेरी का उपयोग करें।
- जेसन ऑब्जेक्ट को स्ट्रिंग में रूपांतरित करने के लिए `dump()` और स्ट्रिंग से जेसन ऑब्जेक्ट बनाने के लिए `parse()` फ़ंक्शन प्रोवाइड करते हैं।
- जेसन अभिलेख में leaf ऑब्जेक्ट जोड़ने के लिए `push_back()` और `emplace_back()` जेसन में तत्काल अंतरण प्र