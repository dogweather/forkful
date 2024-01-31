---
title:                "टेक्स्ट फ़ाइल पढ़ना"
date:                  2024-01-20T17:54:29.698228-07:00
model:                 gpt-4-1106-preview
simple_title:         "टेक्स्ट फ़ाइल पढ़ना"

category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

टेक्स्ट फाइल पढ़ने का मतलब है, फाइल से डेटा लेना और उसे प्रोग्राम में उपयोग करना। प्रोग्रामर्स यह इसलिए करते हैं क्योंकि कई बार कॉन्फ़िगरेशन, डेटा या स्क्रिप्ट्स को टेक्स्ट फाइलों में स्टोर किया जाता है।

## How to: (कैसे करें:)

```C++
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ifstream inputFile("example.txt");
    std::string line;

    if (inputFile.is_open()) {
        while (getline(inputFile, line)) {
            std::cout << line << '\n';
        }
        inputFile.close();
    } else {
        std::cout << "फाइल खोलने में असमर्थ।" << std::endl;
    }

    return 0;
}
```
सैंपल आउटपुट:
```
पहला लाइन
दूसरा लाइन
तीसरा लाइन
```

## Deep Dive (गहराई से जानकारी):

टेक्स्ट फाइल पढ़ने की प्रक्रिया साधारण है, लेकिन पुराने जमाने में यह काफी जटिल था क्योंकि हर सिस्टम की अपनी एन्कोडिंग और फाइल सिस्टम होती थी। C++ में `ifstream` वर्ग का इस्तेमाल करते हुए यह काम आसान हो जाता है। वैकल्पिक तरीकों में `stdio` की `fopen`, `fgets` वगैरह शामिल हैं। आज के आधुनिक C++ में, `ifstream` का उपयोग करना ज्यादा साफ़-सुथरा और ऑब्जेक्ट-ओरिएंटेड तरीका माना जाता है।

## See Also (देखें भी):

- C++ डॉक्युमेंटेशन ऑन `ifstream`: https://en.cppreference.com/w/cpp/io/basic_ifstream
- C++ फाइल हैंडलिंग ट्यूटोरियल: https://www.cplusplus.com/doc/tutorial/files/
- GeeksforGeeks पर फाइल I/O गाइड: https://www.geeksforgeeks.org/file-handling-c-classes/
