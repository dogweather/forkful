---
title:    "C: उपस्थिति उपस्थितियों का जड़ से निकालना"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/c/extracting-substrings.md"
---

{{< edit_this_page >}}

# क्यों
क्या आपने कभी एक नाम के साथ उसके अंत में बार-बार "जूनियर" का शब्द देखा है और आपको उस शब्द को हटाना है? स्ट्रिंग के बीच से एक अंतर को हटाने के लिए हम कई बार substring (उपस्थित पठन) को उपयोग करते हैं। इस ब्लॉग पोस्ट में हम इस उपकला के साथ गहराई से जानेंगे।

# कैसे करें
```c
#include <stdio.h> 
#include <string.h> 

int main() 
{ 
	// दिए गए स्ट्रिंग से एक substring निकालने के लिए 
	char name[] = "लक्ष्य जूनियर"; 
	char sub_name[7]; // नई स्ट्रिंग के लिए ठेंगा आरक्षित करें 

	// लक्ष्य जूनियर से sub_name निकालें 
	// स्ट्रिंग की शुरुआत से 7 वर्ण तक 
	strncpy(sub_name, name, 7); 

	// नए substring को प्रिंट करें 
	printf("substring: %s", sub_name); 

	return 0; 
} 
```
```
Output: 
substring: लक्ष्य
```

# गहराई में जाएं
जब हम स्ट्रिंग से substring को निकालते हैं, तो हमें अपने कोड में ध्यान देने की आवश्यकता होती है कि हम सही स्ट्रिंग से सही अंतर को निकाल रहे हैं। यदि हम स्ट्रिंग की शुरुआत से अंत तक substring निकालते हैं, तो हमें अपने कोड में सीधी रिक्शा का खतरा होता है। इसलिए, हमें अपने कोड में गहराई से देखने की आवश्यकता होती है कि हम अंतर किस स्ट्रिंग से निकाल रहे हैं।

# देखें भी
- [String Functions in C](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [How to Use String Functions in C](https://www.geeksforgeeks.org/string-handling-strings-in-c/)
- [C - Substring function](https://www.programiz.com/c-programming/library-function/string.h/substring)

# देखें भी
- [C में स्ट्रिंग फंक्शन](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [C में स्ट्रिंग फंक्शन का