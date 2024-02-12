---
title:                "CSV के साथ काम करना"
aliases:
- hi/cpp/working-with-csv.md
date:                  2024-02-03T19:19:42.968609-07:00
model:                 gpt-4-0125-preview
simple_title:         "CSV के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

CSV (Comma Separated Values) फाइलों के साथ काम करने का मतलब है डेटा की प्रोसेसिंग और मैनिपुलेशन करना, जो एक साधारण पाठ प्रारूप में संग्रहित होता है, जहां पाठ की प्रत्येक पंक्ति एक तालिका में एक पंक्ति का प्रतिनिधित्व करती है, और कॉमाज व्यक्तिगत कॉलमों को अलग करते हैं। प्रोग्रामर्स इसका उपयोग विभिन्न सिस्टमों के बीच डेटा का आयात, निर्यात, और प्रबंधन करने के लिए करते हैं क्योंकि CSV को एक हल्के, मानव-पठनीय डेटा इंटरचेंज प्रारूप के रूप में व्यापक स्वीकृति प्राप्त है।

## कैसे:

### C++ स्टैंडर्ड लाइब्रेरी का उपयोग करके एक CSV फाइल पढ़ना:

```cpp
#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>

int main() {
    std::ifstream file("data.csv");
    std::string line;
    
    while (std::getline(file, line)) {
        std::stringstream lineStream(line);
        std::string cell;
        std::vector<std::string> parsedRow;
        
        while (std::getline(lineStream, cell, ',')) {
            parsedRow.push_back(cell);
        }
        
        // यहाँ parsedRow को प्रोसेस करें
        for (const auto& val : parsedRow) {
            std::cout << val << "\t";
        }
        std::cout << std::endl;
    }
    
    return 0;
}
```

### एक CSV फाइल में लिखना:

```cpp
#include <fstream>
#include <vector>

int main() {
    std::ofstream file("output.csv");
    std::vector<std::vector<std::string>> data = {
        {"Name", "Age", "City"},
        {"John Doe", "29", "New York"},
        {"Jane Smith", "34", "Los Angeles"}
    };
    
    for (const auto& row : data) {
        for (size_t i = 0; i < row.size(); i++) {
            file << row[i];
            if (i < row.size() - 1) file << ",";
        }
        file << "\n";
    }
    
    return 0;
}
```

### एक तृतीय-पक्ष पुस्तकालय का उपयोग करना: `csv2`:

जबकि C++ स्टैंडर्ड लाइब्रेरी फ़ाइलों और स्ट्रिंग्स के साथ काम करने के लिए बुनियादी उपकरण प्रदान करती है, तृतीय-पक्ष पुस्तकालयों का उपयोग CSV प्रोसेसिंग को सरल बना सकता है। ऐसी ही एक पुस्तकालय है `csv2`, इसके प्रयोग की आसानी और कुशलता के लिए जानी जाती है।

- स्थापना: आमतौर पर Conan जैसे पैकेज मैनेजरों के माध्यम से या सीधे इसके GitHub भंडार से स्थापित किया जाता है।

`csv2` का उपयोग करके एक CSV फाइल पढ़ने का उदाहरण:

```cpp
#include <csv2/reader.hpp>
#include <iostream>

int main() {
    csv2::Reader<csv2::delimiter<','>, csv2::quote_character<'"'>, csv2::first_row_is_header<true>> csv;
    if (csv.mmap("data.csv")) {
        const auto header = csv.header();
        for (const auto row : csv) {
            for (const auto cell : row) {
                std::cout << cell.second << "\t"; // प्रत्येक सेल वैल्यू को प्रिंट करें
            }
            std::cout << std::endl;
        }
    }
    return 0;
}
```

पढ़ने के ऑपरेशन के लिए सैंपल आउटपुट ऐसा दिख सकता है (एक सरल तीन-कॉलम CSV फाइल को मानते हुए):

```
John    29    New York    
Jane    34    Los Angeles
```

ये उदाहरण C++ में मौलिक CSV ऑपरेशन्स को कवर करने के लिए हैं। बड़ी फ़ाइलों के साथ काम करने या जटिल डेटा रूपांतरण जैसे अधिक जटिल परिदृश्यों के लिए, विशेषज्ञ पुस्तकालयों या उपकरणों में अधिक खोज की आवश्यकता हो सकती है।
