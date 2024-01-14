---
title:                "C++: रेग्युलर एक्सप्रेशन का उपयोग करना"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

##एक नज़र में यह क्यों
कोई व्यक्ति सामान्य अभिव्यक्ति का इस्तेमाल क्यों करेगा।

## कैसे करें

अभिव्यक्ति से यूजर के पास रेगुलर एक्सप्रेशन्स का उपयोग करने और मानव-आदि को साधनों का इस्तेमाल करना। इससे उनके कोड आसानी से पढ़ा जा सकता है और कोड के स्टाइल में सुधार आसान होगा।

`` `C ++
#include <iostream>
#include <regex>

using namespace std;

int main() {
  regex regexPattern("a[a-z]{2,4}d");
  string testString = "abcd";
  if (regex_match(testString, regexPattern)) {
    cout << "मेरा कोड aलियो बाद में है: abcd";
  }
  else {
    cout << "मेरा कोड वह सामान्य रूप से a अलावा अन्य किसी चीज़ दिए गए स्ट्रिंग संभव है।";
  }

  return 0;
}
`` `
आउटपुट:
मेरा कोड aलियो बाद में है: abcd

## गहराई पत्नी
रूटार एक भविष्यवक्ता है कि आपको ऑनलाइन एक्सप्रेशन्स के उपयोग के बारे में एक ठोस समझ भी है। अपनी स्थिति और दावा को प्रमाणित करने के लिए विस्तृत जानकारी है। अभिव्यक्ति के साथ आकृति के अध्ययन आपको आणविक उपयोग के लिए इस्तेमाल करें।

देखें भी।
- [उपनिषद्ध कोडिंग इनवोक का स्थान](https://www.invoicely.com/blog/regular-expressions-cheat-sheet/)
- [फ़ूडस के साथ हास्य क्या है](https://www.foodnetwork.com/recipes/chicken-tandoori-tikka-masala-recipe-2043691) सामान्य कांच की बोतल
- [रूट एक ब्लॉगर ने कहा,</मॊपडIVAL](https://blog.r/2/exploring-regex/)
- [पायथन के लिए बार प्रोग्रामिंग।](https://realpython.com/regex-python/)