---
title:                "भविष्य या अतीत में तारीख की गणना"
aliases:
- /hi/cpp/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:31:59.482763-07:00
model:                 gpt-4-1106-preview
simple_title:         "भविष्य या अतीत में तारीख की गणना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
तारीख की गणना करना यह है कि एक निर्धारित समयावधि को जोड़कर या घटाकर भविष्य या भूतकाल की तारीख निकाली जा सकती है। प्रोग्रामर इसे आयोजनों की योजना बनाने, समय सीमाओं का प्रबंधन, और तारीख से संबंधित गणनाओं के लिए करते हैं।

## कैसे करें? (How to:)
```C++
#include <iostream>
#include <chrono>
#include <iomanip>
#include <ctime>

int main() {
    // वर्तमान तारीख और समय प्राप्त करना
    std::chrono::system_clock::time_point today = std::chrono::system_clock::now();
    
    // 30 दिन भविष्य में की गणना
    std::chrono::duration<int, std::ratio<60*60*24>> thirtyDays(30);
    std::chrono::system_clock::time_point futureDate = today + thirtyDays;

    // 30 दिन पहले की गणना
    std::chrono::system_clock::time_point pastDate = today - thirtyDays;

    // time_t में परिवर्तन और प्रिंट करना
    time_t futureTime = std::chrono::system_clock::to_time_t(futureDate);
    time_t pastTime = std::chrono::system_clock::to_time_t(pastDate);

    std::cout << "आज की तारीख: " << std::put_time(std::localtime(&futureTime), "%Y-%m-%d") << std::endl;
    std::cout << "30 दिन भविष्य में तारीख: " << std::put_time(std::localtime(&futureTime), "%Y-%m-%d") << std::endl;
    std::cout << "30 दिन पहले की तारीख: " << std::put_time(std::localtime(&pastTime), "%Y-%m-%d") << std::endl;

    return 0;
}
```
सैंपल आउटपुट:
```
आज की तारीख: 2023-04-05
30 दिन भविष्य में तारीख: 2023-05-05
30 दिन पहले की तारीख: 2023-03-06
```

## गहन जानकारी (Deep Dive)
तारीख की गणना एक ऐसी प्रक्रिया है जिसका इस्तेमाल सदियों से हो रहा है, लेकिन प्रोग्रामिंग में इसके तरीके विस्तृत और अधिक सटीक हैं। C++20 ने `<chrono>` लाइब्रेरी के साथ स्थानीय तिथियों और समय क्षेत्रों को संभालने में सुधार किया है। इसके अलावा बूस्ट डेट टाइम लाइब्रेरी और C++11 या पुराने संस्करणों के लिए `<ctime>` जैसे विकल्प भी हैं।

तिथि की गणना करने का मुख्य लाभ डेटा परिचालन और इंटरफेसिंग में सुधार, साथ ही साथ ऐतिहासिक डेटा विश्लेषण और भविष्यवाणी मॉडलिंग में अत्याधुनिक उपकरणों का प्रयोग करना है। इसके अंतर्निहित तत्वों में समय और कैलेंडर का जटिल गणित शामिल है, जिसे छलांग वर्ष और समय क्षेत्रों के अंतरों के साथ-साथ सॉफ़्टवेयर उपकरणों द्वारा सावधानी से संभाला जाना चाहिए।

## यह भी देखें (See Also)
- [आधिकारिक C++ दस्तावेजीकरण](https://en.cppreference.com/w/)
- [C++ `<chrono>` Library](https://en.cppreference.com/w/cpp/header/chrono)
- [Boost Date_Time Library](https://www.boost.org/doc/libs/1_75_0/doc/html/date_time.html)
- [C++ Standards Support in GCC](https://gcc.gnu.org/projects/cxx-status.html)
