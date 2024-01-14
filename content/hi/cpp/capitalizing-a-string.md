---
title:    "C++: स्ट्रिंग को बड़े अक्षरों में परिवर्तित करना"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# वजह

एक लचीले स्ट्रिंग को कैपिटलाइज करने के लिए कोई क्यों करे? 

## कैसे करें

```
C++ 
#include <iostream> 
#include <string> 

using namespace std; 

int main() 
{ 
  string str = "hindi blog"; 
  
  // capitalize the first letter of each word 
  for (int i=0; i<str.length(); i++) 
  { 
    // if current character is space 
    if (i == 0 || str[i-1] == ' ') 
      str[i] = toupper(str[i]); // convert to uppercase 
  } 
  
  cout << str; // output: Hindi Blog
  return 0; 
} 
```

## डीप डाइव

कैपिटलाइज़ फ़ंक्शन एक बहुत ही उपयोगी फ़ंक्शन है जो की स्ट्रिंग में एक लाइन या शब्द को uppercase में बदल देती है। यह स्ट्रिंग को मॉडीफ़ाई करती है और इसे लूप के द्वारा लागू किया जाता है। आप स्ट्रिंग के प्रत्येक आइटम को चेक कर सकते हैं और उसको uppercase में बदल सकते हैं। इस फ़ंक्शन के साथ आप टेक्स्ट आधारित गेम्स बना सकते हैं या शब्दों को स्विच कर सकते हैं। 

# देखें भी

- [C++ string फ़ॉर्मेटिंग](https://www.programiz.com/cpp-programming/library-function/cctype/toupper)
- [एक स्ट्रिंग को uppercase करने के तरीके](https://www.geeksforgeeks.org/how-to-capitalize-the-first-letter-of-each-word-in-a-string-in-cpp/)
- [स्ट्रिंग कैसे मॉडीफ़ाई करें](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm)