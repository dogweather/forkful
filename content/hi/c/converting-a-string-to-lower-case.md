---
title:    "C: लोअर केस में स्ट्रिंग को कनवर्ट करना"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्यों

चलिए देखें कि क्यों हम एक स्ट्रिंग को निचले अक्षर में बदलने में दिलचस्पी रखते हैं।

## कैसे करें

इस समस्या को हल करने के लिए, हम स्ट्रिंग को निचले अक्षर में बदलने के लिए एक फ़ंक्शन का निर्माण करेंगे। यहां हम `tolower()` फ़ंक्शन का उपयोग करेंगे जो दो तरीकों से काम कर सकता है:

```C
#include <stdio.h>
#include <ctype.h>

// Function to convert a string to lower case
void toLower(char *str){
    // Loop through each character in the string
    for(int i=0; str[i] != '\0'; i++){
        // Convert each character to lower case using tolower() function
        str[i] = tolower(str[i]);
    }
}

int main(){
    // Sample string
    char str[] = "Hello World";
    
    // Call toLower() function
    toLower(str);
    
    // Print the converted string
    printf("%s", str);
    
    return 0;
}

// Output: hello world
```

## गहराई में जाएं

इस समस्या को हल करने के लिए एक साधारण फ़ंक्शन का इस्तेमाल करने के अलावा, हम एक `tolower()` के जानकारी का भी उपयोग कर सकते हैं। यह एक सामान्य लाइब्रेरी फ़ंक्शन है जो दिए गए अक्षर को निचले अक्षर में बदलता है। यदि आप अपनी स्वयं की फ़ंक्शन बनाना चाहते हैं, तो आपको उपरोक्त उदाहरण की तरह फ़ंक्शन में एक लूप जोड़ना होगा जो दिया गया स्ट्रिंग प्रोसेस करेगा।

अधिक जानकारी के लिए, आप ये लिंक्स देख सकते हैं:

स्ट्रिंग्स: https://www.geeksforgeeks.org/string-manipulation-c-2/
tolower() फ़ंक्शन: https://www.tutorialspoint.com/c_standard_library/c_function_tolower.htm

## देखें भी

लिखाबधोक फ़ंक्शन: https://www.geeksforgeeks.org/strlwr-function-string-h/
कुंजी धारक देवनागरी समस्या: https://www.tutorialspoint.com/keyboard_arrow_upward.html