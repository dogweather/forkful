---
title:                "समझना कमांड लाइन तर्कों को"
html_title:           "C++: समझना कमांड लाइन तर्कों को"
simple_title:         "समझना कमांड लाइन तर्कों को"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

कमांड लाइन आर्गुमेंट पढ़ना एक उपयोगी कौशल है जो कोड को वार्तालाप मतलब के साथ आरम्भ करने में मदद करता है। यह प्रोग्रामर्स द्वारा सुनिश्चित करने के लिए किया जाता है कि उनके कोड सही समय पर ठीक तरीके से चल रहा है।

## कैसे करें:

```C++
#include <iostream>

int main(int argc, char* argv[]) {
    // कमांड लाइन आर्गुमेंट पढ़ें
    for (int i = 1; i < argc; i++) {
        std::cout << "कमांड लाइन आर्गुमेंट " << i << ": " << argv[i] << std::endl;
    }

    return 0;
}
```

### उत्पाद का उदाहरण:

जब इस कोड को एक फाइल में "command_arguments.cpp" नाम सहित सहेजा जाता है और "hello" और "world" जैसे दो कमांड लाइन आर्गुमेंट दिए गए हैं, तो उत्पाद निम्नलिखित रूप में है:
```
कमांड लाइन आर्गुमेंट 1: hello
कमांड लाइन आर्गुमेंट 2: world
```

## गहराई में जाएं:

इस कौशल को आपके द्वारा दी गई तुलना की मदद से ज्यादा समझने के लिए, आप प्राचीन संदर्भ, वैकलपिक विकल्प और फाईल स्नेक और दृश्य से संबद्ध स्रोतों का पता लगा सकते हैं।

## देखें भी:

- [कमांड लाइन आर्गुमेंट्स से काम करना: एक संबंधित गाइड](https://docs.microsoft.com/en-us/cpp/cpp/parsing-cpp-command-line-arguments?view=msvc-160)
- [प्रोग्राम लाइन आर्गुमेंट में उपयोगी संख्या गणना करने के लिए एक उपयोगी जांच सुझाव](https://www.educative.io/edpresso/how-to-count-the-number-of-words-in-a-string-in-cpp)