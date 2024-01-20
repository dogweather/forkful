---
title:                "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
html_title:           "Kotlin: एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
simple_title:         "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# एक स्ट्रिंग को लोअर केस में बदलना: एक सी ++ (वर्तमान संस्करण) प्रोग्रामिंग मार्गदर्शिका (C++ Programming Guide)

## क्या और क्यों ?
स्ट्रिंग को लोअर केस में बदलने का अर्थ होता है उसमें मौजूद सभी कैपिटल अक्षरों को छोटे अक्षरों में बदलना। इसे प्रोग्रामर्स इसलिए करते हैं ताकि वे कोड के संवेदनशीलता को कम कर सकें और डाटा मिलान और मिलाप सरल बना सके।

## कैसे :

लोअर केस के लिए स्ट्रिंग को कन्वर्ट करने के लिए कोडिंग उदाहरण।

```C++
#include <iostream> 
#include <algorithm>
#include <cctype>

int main()
{
	std::string s = "Hello World";
	std::transform(s.begin(), s.end(), s.begin(),
		[](unsigned char c) { return std::tolower(c); });
	std::cout << s;
}
``` 
इस कोड के निष्पादन पर आपको "hello world" प्राप्त होगा।

## गहराई में :
हम जब दो स्ट्रिंग्स को मिलाते हैं हमें यह सुनिश्चित करना पड़ता है कि उनका केस (कैप्स या स्मॉल) समान हो। यदि यह लोअर केस में है, तो हमें डाटा को मिलाने और खोजने में आसानी होती है। एल्टर्नेटिवली, आप `boost::algorithm::to_lower(str)` का उपयोग भी कर सकते हैं, जो `Boost` लाइब्रेरी का एक हिस्सा है। 

## भी देखें :

1. [std::transform](http://www.cplusplus.com/reference/algorithm/transform/)
2. [std::tolower](http://www.cplusplus.com/reference/cctype/tolower/)
3. [boost::algorithm::to_lower](https://www.boost.org/doc/libs/1_71_0/doc/html/boost/algorithm/to_lower.html)
   
तो बस यही कारण है जिसके कारण स्ट्रिंग को लोअर केस में बदलना महत्वपूर्ण होता है और सी ++ (वर्तमान संस्करण) में इसे कैसे बदलने का तरीका भी। आशा करता हूँ आपको इस लेख का अनुवाद पसंद आया होगा!