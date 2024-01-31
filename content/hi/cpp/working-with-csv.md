---
title:                "CSV के साथ काम करना"
date:                  2024-01-19
simple_title:         "CSV के साथ काम करना"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

# CSV का काम क्या है और क्यों?

CSV, यानी Comma-Separated Values, एक सरल फ़ाइल प्रारूप है जिसे कंप्यूटर प्रोग्रामिंग में डेटा संग्रहीत और साझा करने के लिए इस्तेमाल किया जाता है। प्रोग्रामर्स इसे डेटा एक्सचेंज के लिए उपयोग करते हैं क्योंकि यह हल्का, पठनीय और आसानी से परिवर्तित होता है।

# कैसे करें:

```C++
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>

// CSV फ़ाइल पढ़ना
void readCSV(std::istream &input) {
    std::string csvLine;
    while (getline(input, csvLine)) {
        std::stringstream ss(csvLine);
        std::string data;
        while (getline(ss, data, ',')) {
            std::cout << data << " ";
        }
        std::cout << std::endl;
    }
}

// CSV फ़ाइल लिखना
void writeCSV(std::ostream &output, const std::vector<std::vector<std::string>> &data) {
    for (const auto &row : data) {
        for (auto cell = row.begin(); cell != row.end(); ++cell) {
            output << *cell;
            if (next(cell) != row.end()) output << ",";
        }
        output << '\n';
    }
}

int main() {
    // CSV फ़ाइल पढ़ने का उदाहरण
    std::ifstream file("example.csv");
    std::cout << "Reading CSV file:" << std::endl;
    readCSV(file);

    // CSV लिखने का उदाहरण
    std::vector<std::vector<std::string>> dataToWrite = {
        {"नाम", "उम्र", "शहर"},
        {"राम", "25", "दिल्ली"},
        {"सीता", "23", "पुणे"}
    };
    std::ofstream outFile("output.csv");
    std::cout << "\nWriting to CSV file:" << std::endl;
    writeCSV(outFile, dataToWrite);
}
```

ऊपर कोड में, पहले हमने `readCSV` फंक्शन बनाया जो इनपुट स्ट्रीम से CSV डेटा पढ़ता है। फिर `writeCSV` फंक्शन है जो डेटा को CSV फ़ॉर्मेट में आउटपुट स्ट्रीम पर लिखता है।

# गहराई से जानकारी:

CSV फ़ाइलें 1970 के दशक से उपयोग में हैं। यह सामान्यत: डेटाबेस और स्प्रेडशीट्स के बीच डेटा अंतरण के लिए इस्तेमाल किया जाता है। JSON और XML जैसे अन्य डेटा स्वारूप भी प्रचलित हैं, लेकिन CSV की सादगी के कारण यह खासा लोकप्रिय रहता है। C++ में CSV से निपटने का कोई मानकीकृत तरीका नहीं है, इसलिए हमें इसे मैन्युअली पढ़ना और लिखना पड़ता है।

# सम्बंधित जानकारी:

- C++ के स्टैंडर्ड टेम्प्लेट लाइब्रेरी (STL) के बारे में अधिक जानने के लिए: [cppreference.com](https://en.cppreference.com/w/)
- CSV पार्सिंग लाइब्रेरीज़ के बारे में अधिक जानकारी: [GitHub](https://github.com/search?q=csv+parser+C%2B%2B)
- CSV फ़ाइल प्रारूप के बारे में और अधिक जानने के लिए: [RFC 4180](https://tools.ietf.org/html/rfc4180)
