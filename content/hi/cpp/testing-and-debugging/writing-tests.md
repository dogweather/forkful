---
title:                "टेस्ट लिखना"
aliases: - /hi/cpp/writing-tests.md
date:                  2024-02-03T19:31:25.408742-07:00
model:                 gpt-4-0125-preview
simple_title:         "टेस्ट लिखना"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

C++ में टेस्ट लिखने का अर्थ है छोटे, स्वयं-संगति वाले प्रोग्रामों का निर्माण करना जो आपके कोडबेस के विभाजनों के व्यवहार को स्वचालित रूप से सत्यापित करते हैं। प्रोग्रामर इसे यह सुनिश्चित करने के लिए करते हैं कि उनका कोड अपेक्षित अनुसार काम करे, पिछड़ापन से बचें (अर्थात्, नए परिवर्तन मौजूदा कार्यक्षमता को तोड़ते हैं), और समय के साथ बनाए रखने योग्य कोडबेस को सुविधाजनक बनाने के लिए।

## कैसे:

### गूगल टेस्ट फ्रेमवर्क का उपयोग करते हुए

C++ में टेस्ट लिखने के लिए सबसे लोकप्रिय तृतीय-पक्ष पुस्तकालयों में से एक गूगल टेस्ट है। पहले, आपको गूगल टेस्ट स्थापित करने और इसे अपनी परियोजना के साथ लिंक करने की आवश्यकता होगी। सेट अप हो जाने के बाद, आप टेस्ट मामले लिखना शुरू कर सकते हैं।

```cpp
#include <gtest/gtest.h>

int add(int a, int b) {
    return a + b;
}

TEST(TestSuiteName, TestName) {
    EXPECT_EQ(3, add(1, 2));
}

int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
```

कोड को एक फाइल में सेव करें, और इसे g++ कंपाइलर के साथ कंपाइल करें, गूगल टेस्ट लाइब्रेरी को लिंक करते हुए। यदि सब कुछ सही ढंग से सेट अप किया गया है, तो उत्पन्न निष्पादनयोग्य को चलाने से टेस्ट चलेगा, और यदि `add` फंक्शन अपेक्षित अनुसार काम करता है, तो आप कुछ इस तरह देखेंगे:

```
[==========] Running 1 test from 1 test suite.
[----------] Global test environment set-up.
[----------] 1 test from TestSuiteName
[ RUN      ] TestSuiteName.TestName
[       OK ] TestSuiteName.TestName (0 ms)
[----------] 1 test from TestSuiteName (0 ms total)

[==========] 1 test from 1 test suite ran. (1 ms total)
[  PASSED  ] 1 test.
```

### कैच2 का उपयोग करते हुए

C++ के लिए एक और लोकप्रिय टेस्टिंग फ्रेमवर्क कैच2 है। इसमें सरल सिंटैक्स होता है और आमतौर पर इसे लाइब्रेरी के खिलाफ लिंक करने की आवश्यकता नहीं होती है (केवल-हेडर)। यहां कैच2 के साथ एक सरल टेस्ट लिखने का एक उदाहरण दिया गया है:

```cpp
#define CATCH_CONFIG_MAIN  // This tells Catch to provide a main() - only do this in one cpp file
#include <catch.hpp>

int multiply(int a, int b) {
    return a * b;
}

TEST_CASE( "Integers are multiplied", "[multiply]" ) {
    REQUIRE( multiply(2, 3) == 6 );
}
```

इस टैस्ट को कंपाइलिंग और चलाने पर, कैच2 एक स्पष्ट आउटपुट प्रदान करता है जो इंगित करता है क्या टैस्ट पास हुआ या फेल, साथ ही विफलताओं को डीबग करने के लिए आवश्यक किसी भी जानकारी के साथ:

```
===============================================================================
All tests passed (1 assertion in 1 test case)
```

ये उदाहरण यह दिखाते हैं कि कैसे अपने C++ विकास कार्यप्रवाह में टेस्टिंग फ्रेमवर्कों को एकीकृत करने से आपके कोड की विश्वसनीयता और रखरखाव योग्यता में महत्वपूर्ण रूप से बढ़ोतरी हो सकती है।
