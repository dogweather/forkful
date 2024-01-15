---
title:                "CSV के साथ काम करना"
html_title:           "C++: CSV के साथ काम करना"
simple_title:         "CSV के साथ काम करना"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## क्यों
जैसा कि हम सब जानते हैं, CSV (Comma Separated Values) फ़ाइलें डेटा को आसानी से संग्रहीत करने और देखने की सबसे पसंदीदा तकनीक हैं। CSV फ़ाइलों का प्रयोग ज्यादातर स्प्रेडशीट ऐप्स द्वारा किया जाता है जो आमतौर पर डेटा को टैबल्स में दर्शाते हैं। लेकिन यह भी एक बहुत ही उपयोगी तकनीक है जो की C++ में भी बड़े से बड़े एप्लिकेशन्स में उपयोग में लाई जा सकती है।

## कैसे
यदि आप C++ में CSV फ़ाइलों के साथ काम करना चाहते हैं, तो आपको सबसे पहले सीएसवी लाइब्रेरी को अपने कोड में शामिल करना होगा। फिर आप ```ifstream``` और ```getline()``` का प्रयोग कर सकते हैं ताकि आप फ़ाइल से डेटा ला सकें। अगर आप सारे डेटा को एक ही वैरिएबल में संग्रहीत करना चाहते हैं, तो आप ```vector``` का भी प्रयोग कर सकते हैं। नीचे दिए गए कोड ब्लॉक आपको इन विधियों को समझाएंगे और साथ ही आपको सैंपल आउटपुट भी देखने को मिलेगा।

```C++
#include <iostream>
#include <fstream>
#include <vector>

using namespace std;

int main() {

    ifstream file("sample.csv"); // आपकी CSV फ़ाइल का नाम यहां बदलें।

    vector<string> data; // सारे डेटा को संग्रहीत करने के लिए एक नया वैरिएबल बनाएं।

    string row; // प्रत्येक पंक्ति को स्ट्रिंग में संग्रहीत करने के लिए एक और वैरिएबल बनाएं।

    while (getline(file, row)) {
        data.push_back(row); // पंक्तियों को वैक्टर में जोड़ें।
    }

    for (string row : data) {
        cout << row << endl; // वैक्टर म