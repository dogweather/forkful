---
title:                "पाठ की खोज और प्रतिस्थापन"
html_title:           "Bash: पाठ की खोज और प्रतिस्थापन"
simple_title:         "पाठ की खोज और प्रतिस्थापन"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
सबसे पहले, हमें खोजने और पाठ को बदलने की क्या जरूरत होती है? यह एक ऐसा तरीका है जिससे हम एक विशेष श्रृंखला को दूसरी या नई श्रृंखला से बदल सकते हैं। यह उस समय आवश्यक होता है जब हमें एक बड़े कोड में किसी विशेष शब्द को खोजना और बदलना है।

## कैसे (How to:)
हम इसे C++ में "std::string::replace" और "std::string::find" की मदद से कर सकते हैं।

```C++
#include <iostream>
#include <string>
 
int main(){
    std::string s = "मैं जानता हूं कि आप ऐसा कर सकते हैं।";
    size_t pos = s.find("ऐसा");
    
    if (pos != std::string::npos)
        s.replace(pos, 2, "ऐसे");
    std::cout << s << std::endl;
    return 0;
}
```
उत्पादन होगा: "मैं जानता हूं कि आप ऐसे कर सकते हैं।"

## गहरी जांच (Deep Dive)
C++ में "std::string::replace" और "std::string::find" की कामना १९९८ के ISO C++ मानक के साथ आई। इससे पहले, प्रोग्रामरों को खुद कोड लिखना पड़ता था। वैकल्पिक फ़ंक्शन जैसे "boost::algorithm::replace_all" और अन्य ऐसे कार्य जो इसे localized/Unicode strings के साथ काम करने के लिए सुधार सकते हैं।

## और भी देखें (See Also)
- *std::string* के बारे में और जानें: http://www.cplusplus.com/reference/string/string/ 
- *Boost String Algorithm Library*: https://www.boost.org/doc/libs/1_73_0/doc/html/string_algo.html
- *C++ और Unicode* के बारे में: https://stackoverflow.com/questions/1049947/what-is-the-best-way-to-use-unicode-string-in-c-today