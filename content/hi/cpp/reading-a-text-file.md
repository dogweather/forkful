---
title:                "एक पाठ फ़ाइल पढ़ना"
html_title:           "Bash: एक पाठ फ़ाइल पढ़ना"
simple_title:         "एक पाठ फ़ाइल पढ़ना"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

टेक्स्ट फ़ाइल को पढ़ना मतलब है, उसकी सामग्री को कॉड में इस्तेमाल करने के लिए लाना। प्रोग्रामर्स इसे डेटा मॉडलिंग, फ़ाइल मैनिपुलेशन और बनाये गए कॉड के परिणामों के विश्लेषण के लिए करते हैं। 

## कैसे करें:
```C++
#include <iostream>
#include <fstream>
#include <string>

int main()
{
    std::string line;
    std::ifstream file("example.txt");

    if (file.is_open())
    {
        while (getline(file, line))
        {
            std::cout << line << '\n';
        }
        file.close();
    }

    else std::cout << "Unable to open the file";

    return 0;
}
```

ऊपरी कोड पूरी फ़ाइल की लाइनों को पढ़ता है और प्रत्येक लाइन को न्यायवादी रेखा पर प्रिंट करता है। “Unable to open the file” संदेश निष्पादित होता है यदि फ़ाइल उपलब्ध नहीं होती है या उसे खोलने का प्रयास करने पर त्रुटि होती है।

## गहराई में:

टेक्स्ट फ़ाइल को पढ़ना आमतौर पर C++ के आधारभूत्त IO बाइबलियों का एक अहम हिस्सा है। यह आसान है और वर्षों से इस्तेमाल हो रहा है।

वैकल्पिक रूप से, आप `boost` में `fstream` जैसी उच्च स्तरीय बाइबलिया भी इस्तेमाल कर सकते हैं। यहां पर, `std::getline` विधि इस्तेमाल की जाती है जो `std::string` प्रकार की किसी भी इकाई को पढ़ने के लिए एक सामग्रीक विधि है।

## यह भी देखें:

- [C++ डॉक्युमेंटेशन: इनपुट/आउटपुट बाइबलिया](http://www.cplusplus.com/reference/iostream/)
- [C++ ट्यूटोरियल: टेक्स्ट फ़ाइलस से डेटा पठने](https://www.cplusplus.com/doc/tutorial/files/)
- [Boost बाइबलिया](https://www.boost.org/doc/libs/1_72_0/libs/iostreams/doc/index.html)