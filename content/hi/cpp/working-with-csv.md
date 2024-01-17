---
title:                "कंप्यूटर प्रोग्रामिंग में 'csv' का उपयोग करना"
html_title:           "C++: कंप्यूटर प्रोग्रामिंग में 'csv' का उपयोग करना"
simple_title:         "कंप्यूटर प्रोग्रामिंग में 'csv' का उपयोग करना"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

एक CSV के साथ काम करने से तात्पर्य उन स्ट्रिंग्स को हैं जो इंटरकॉन्सेट कॉमा से अलग हैं। कंप्यूटर और आप को इस प्रकार के डेटा को संरचित तरीके से लिखने और पढ़ने की अनुमति देता है। प्रोग्रामर इसे करते हैं ताकि उन्हें विभिन्न सॉफ्टवेयर या साइटों के बीच डेटा शेयर करने में आसानी हो।

## कैसे करें:

```C++
#include <iostream>
#include <fstream>
#include <string>
#include <vector>

using namespace std;

int main() {
    // CSV फाइल से डेटा पढ़ें
    ifstream file("example.csv");
    string line;
    vector<vector<string>> data; // संग्रहीत डेटा के लिए एक बहुमुखी एरे

    while (getline(file, line)) {
        vector<string> row; // पंक्ति के लिए एक एरे
        string token;
        size_t pos = 0;

        // सभी स्ट्रिंग को कोमा से विभाजित करें और पंक्ति में संग्रहीत करें
        while ((pos = line.find(',')) != string::npos) {
            token = line.substr(0, pos);
            row.push_back(token);
            line.erase(0, pos + 1);
        }

        // अंतिम स्ट्रिंग संग्रहित करें
        row.push_back(line);
        // पंक्ति को संग्रहित करें
        data.push_back(row);
    }

    // संग्रहित डेटा को प्रिंट करें
    for (vector<string> row : data) {
        for (string item : row) {
            cout << item << " ";
        }
        cout << endl;
    }

    // नए CSV फाइल लिखें
    ofstream outfile ("new_example.csv");

    for (vector<string> row : data) {
        for (string item : row) {
            // कोमा से अलग करें और फाइल में लिखें
            outfile << item << ",";
        }
        // अंतिम कोमा को हटाएं
        outfile << endl;
    }

    // फाइल बंद करें
    file.close();
    outfile.close();
    
    return 0;
}
```

उपरोक्त कोड में, हम संग्रहीत डेटा को ```data``` एरे में रखते हैं और ```getline()``` फ़ंक्शन का उपयोग करके फाइल से पंक्तियाँ पढ़ते हैं। स्ट्रिंग को कोमा से विभाजित करने के लिए हम ```find()``` और ```substr()``` फ़ंक्शन का उपयोग करते हैं। अंत में, हम दोबारा एक नई CSV फाइल बनाते हैं जिसमें संग्रहीत डेटा होता है।

## गहराईग्रहण (Deep Dive):

CSV, कॉमा द्वारा विभक्तित वस्तुओं का संग्रहण करने का एक प्राचीन विधान है। इंटरनेट का उद्भव होने से पहले भी, स्प्रेडशीट प्रोग्राम अक्सर CSV फाइल्स को समर्थन करते थे। आज, एक लाइन में कुछ सैमिट के साथ विभक्तित करने की आवश्यकता के कारण, यह अनेक हार्डवेयर प्लेटफॉर्मों पर अपना स्थान बनाए हुए है। इसके अलावा, XML और JSON जैसे अन्य संरचनाओं का भी इस्तेमाल किया जाता है लेकिन CSV की सादगी और कम फ़ाइल साइज के कारण यह आज भी यादगार ह