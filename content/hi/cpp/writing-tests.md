---
title:                "C++: टेस्ट लेखन"
programming_language: "C++"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## क्यों
अगर आप सही समय पर अपने आप को बनाएं तो यह आपको कोड के लिए समय बचाएगा। इसलिए, टेस्ट लिखने का काम फायदेमंद हो सकता है।

## कैसे करें

```C++
#include <iostream>

using namespace std;

int multiply(int a, int b){
    return a * b;
}

int main(){
    // नए कोड का परीक्षण करें
    int result = multiply(5, 7);
    
    // परीक्षा का परिणाम मान के साथ तुलना करें
    if(result == 35){
        cout << "सफलतापूर्वक परीक्षण किया गया!";
    }
    else{
        cout << "परीक्षण असफल हुआ।";
    }
    
    return 0;
}
```
### उत्पाद
```सफलतापूर्वक परीक्षण किया गया!```

## गहराई में जाएँ
टेस्ट कोड प्रोजेक्ट में अत्यधिक महत्वपूर्ण होता है। यह आपको सुनिश्चित करता है कि आपके कोड में कोई त्रुटियां नहीं हैं और आपका कोड स्वस्थ है। इससे आपके उपभोक्ताओं को भी आपके सॉफ्टवेयर पर भरोसा बना रहता है।

## देखें भी
- [वीडियो: टेस्ट लिखने का कैसे](https://www.youtube.com/watch?v=9oJ5xBSDRek)
- [लेख: सी++ में टेस्ट कैसे लिखें](https://www.geeksforgeeks.org/writing-test-cpp/)