---
title:                "कमांड लाइन तर्कों को पढ़ना"
html_title:           "Kotlin: कमांड लाइन तर्कों को पढ़ना"
simple_title:         "कमांड लाइन तर्कों को पढ़ना"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)

कमांड लाइन आर्गुमेंट्स पढ़ना (reading command line arguments) एक प्रक्रिया है, जिसमें हमεं कमांड लाइन से प्रोग्राम में निर्देश प्राप्त होते हैं। प्रोग्रामर्स इसे प्रॉग्राम को अधिक उपयोगी और लचीला बनाने के लिए करते हैं।

## कैसे: (How to)

```C++
#include <iostream>

int main (int argc, char *argv[])  
{
  std::cout << "You've entered " << argc << " arguments:\n";
  
  for (int i = 0; i < argc; ++i)
    std::cout << argv[i] << "\n";

  return 0;
}
```
इस सादे कोड का उत्तर :
```C++
You've entered 3 arguments:
./program
argument1
argument2
```
इसमें argv एक ऐरे है जिसमें आर्गुमेंट्स रखे जाते हैं और argc उनकी संख्या होती है।

## गहरा अध्ययन: (Deep Dive)

1. ऐतिहासिक प्रक्षेपण: पहले कमाणड लाइन आर्गुमेंट्स बड़े अनिवार्य समझे जाते थे, लेकिन आजकल ये उपयोगीता के बिना UI में बदलने के लिए उपयोग होते हैं।
2. विकल्प: getopt(), boost.program_options इत्यादि पुस्तिकाओं का भी उपयोग किया जा सकता है।
3. विषयांतर का विवरण: argc और argv मूल प्रोग्राम के main() फ़ंक्शन से पास होते हैं, जो C++ खोजने के द्वारा सेट होते हैं।

## यह भी देखें: (See Also)

1. C++ की ऑफ़िशियल डॉक्युमेंटेशन जहां पैरामीटर्स को डिक्लेयर करने का तरीका फ़्लेक्सिबल है: https://en.cppreference.com/w/cpp/language/main_function
2. ट्यूटोरियल द्वारा C++ के आर्गुमेंट्स पढ़ने: https://www.tutorialspoint.com/cplusplus/cpp_command_line_arguments.htm