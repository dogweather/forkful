---
title:                "स्ट्रिंग को लोअर केस में बदलें"
html_title:           "C++: स्ट्रिंग को लोअर केस में बदलें"
simple_title:         "स्ट्रिंग को लोअर केस में बदलें"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
किसी स्ट्रिंग को लोअर केस में बदलना, उसके अक्सर कई उपयोग होते हैं। जैसे कि रूपरेखा सामने आने में आसान होती है, मानवीय एक्रोनामिक कोडिंग को आसान बनाने के लिए, या स्ट्रिंग में किसी विशिष्ट पद को ढूंढने के लिए। एक शांत और स्पष्ट उपयोग कस्टम के लिए ऐसे कई मुख्य रूप से काम आते हैं।

## कैसे करें:
```C++
#include <iostream>
#include <string>

using namespace std;

int main(){
  string word = "HINDI";
  
  // एक स्ट्रिंग को लोअर केस में बदलने का प्रक्रिया
  for(int i=0; i<word.length(); i++){
    word[i] = tolower(word[i]);
  }
  
  cout << word << endl;
  // आउटपुट: hindi
  
  return 0;
}
```
मुख्य लूप बहुत सरल है। एक स्ट्रिंग में हर अक्षर को बदलने के लिए, हम उसके अक्षरों के संख्या को जाँचते हैं (इसे करने के लिए string का length() फ़ंक्शन हमारे लिए उपलब्ध होता है) और उसे उसके अक्षरों को दोबारा लिखने के लिए for लूप में प्रवेश करते हैं।

## गहराई में ढूंढें:
स्ट्रिंग को लोअर केस में बदलने की इतिहास, स्ट्रिंगों को फ़ॉर्मैट करने के कई अन्य तरीकों के साथ स्ट्रिंग में से अक्षरों को निकालने के लिए कुछ लाभप्रद है। कुछ भोले से तरीकों के बारे में convert करने का उपयोग के बारे में सूचना उपलब्ध है, और जब हम टेक्निकल स्टाज़ दिखाते हैं, तो हम नए उपयोगकर्ताओं की ओर से योग्य सुझाव प्रदान कर सकते हैं।

## और भी देखें:
- [C++ String Functions](https://www.programiz.com/cpp-programming/library-function/string)
- [C++ tolower() function](https://www.programiz.com/cpp-programming/library-function/cctype/tolower)