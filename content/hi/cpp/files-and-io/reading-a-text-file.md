---
date: 2024-01-20 17:54:29.698228-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) ."
lastmod: '2024-03-13T22:44:52.875216-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093C\u093E\u0907\u0932\
  \ \u092A\u0922\u093C\u0928\u093E"
weight: 22
---

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
