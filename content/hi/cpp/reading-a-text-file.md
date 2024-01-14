---
title:                "C++: एक टेक्स्ट फाइल पढ़ना"
simple_title:         "एक टेक्स्ट फाइल पढ़ना"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## क्यों

एक टेक्स्ट फ़ाइल पढ़ने का क्या महत्व है? अगर आप अपने कोड में डेटा को ठीक से संरचित करना चाहते हैं या कोड रिपोर्ट को जांचने के लिए अपने प्रोग्राम को डेबग करना चाहते हैं, तो टेक्स्ट फ़ाइल पढ़ना अत्यधिक उपयोगी हो सकता है।

## कैसे करें

```C++
#include <iostream>
#include <fstream>
using namespace std;

int main() {
    // Create an input file stream object
    ifstream inputFile;
    // Open the text file
    inputFile.open("sample.txt");

    // Check if file opened successfully
    if (inputFile.is_open()) {
        // Read the file line by line
        string line;
        while (getline(inputFile, line)) {
            // Print the line
            cout << line << endl;
        }
        // Close the file
        inputFile.close();
    }
    else {
        // Print an error message if file cannot be opened
        cerr << "Unable to open file." << endl;
    }

    return 0;
}
```

**उत्पाद:**

```
This is a line of text.
This is another line of text.
```

## गहराई में जाईए

टेक्स्ट फ़ाइल पढ़ना बहुत सरल हो सकता है, लेकिन यह कुछ महत्वपूर्ण बिंदुओं पर ध्यान देने का समय लेता है। पहले, आपको फ़ाइल ओपन करने के बाद उसे सही ढंग से बंद करना होता है, अन्यथा यह आपके सिस्टम पर उठाए गए मामलों को अनापत्ति कर सकता है। दूसरे, आपको स्ट्रिंग के पाठन के अलावा भी एक सीमित संख्या के शब्दों को पढ़ने की क्षमता को विकसित करने के लिए शामिल होना चाहिए।

## और भी देखें

[प्रोग्रामिंग में C++ का कितना महत्वपूर्ण है?](https://www.educba.com/importance-of-c-plus-plus-programming/)

[अपने C++ कोड को ठीक से तरीके से संरचित कैसे करें](https://www.geeksforgeeks.org/structures-in-cpp/)

[अपने C++ कोड को डेबग कैसे करें? ](https://www.geeksforgeeks.org/debugging-in-cpp/)