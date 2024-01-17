---
title:                "दो तारीखों की तुलना"
html_title:           "C++: दो तारीखों की तुलना"
simple_title:         "दो तारीखों की तुलना"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्या और क्यों? 
दो तारीखों की तुलना करना क्या है और क्यों प्रोग्रामर्स इसे करते हैं? प्रोग्रामर्स दो तारीखों की तुलना करें ताकि वे दो अलग तारीखों के बीच अंतर को ध्यान से जांच सकें और जोड़ सकें। इससे कार्यक्रमों में गलती की संभावना कम होती है और समय की बचत होती है।

## कैसे करें: 
```C++
#include <iostream>
using namespace std;

int main() {
  // तारीखों को समान बनाने के लिए मूल्य घोषित करें 
  int firstDate = 20210811;
  int secondDate = 20200702;

  if (firstDate > secondDate) {
    cout << "पहली तारीख बड़ी है।" << endl;
  } else if (firstDate < secondDate) {
    cout << "दूसरी तारीख बड़ी है।" << endl;
  } else {
    cout << "पहली और दूसरी तारीख समान हैं।" << endl;
  }
  
  return 0;
}
```

Output:
``` 
पहली तारीख बड़ी है।
```

## गहराई में जाएं: 
एक और दृष्टिकोण से, इन तारीखों की तुलना का इतिहास हमारे समय के पहले की घटनाओं से जुड़ा हुआ है। आजकल इस कार्य को कम समय में करने के लिए और अधिक सुगम बनाने के लिए संसाधनों के रूप में कंप्यूटर प्रोग्रामिंग सहायक गणित विधि भी उपलब्ध है। तारीखों की तुलना को पहले खारिज कर दिया जाता था जब इसे अलग तरह से सूचित किया जाता था। साधारणतया, तारीख स्पष्ट त्योहार की शुरुआत या संविधानात्मक प्रक्रियाओं को जानने के लिए लिए इस्तेमाल की जाती है।

अलग तारीखों की तुलना के अल्टरनेटिव में, मास के दौरान हालिया दिवसों का अंतर देखना भी आमतौर पर अपनाया जाता है। कुछ प्रोग्रामिंग भाषाओं में, तारीखों की तुलना के लिए विशिष्ट फ़ंक्शन सुलभ होते हैं जो भविष्य में होने वाली घटनाओं को भी प्रभावित कर सकते हैं।

## जानें भी: 
विस्तृत जानकारी और उदाहरण लेने के लिए ये लिंक देखें:
- [GeeksforGeeks - Comparing Dates in C++](https://www.geeksforgeeks.org/comparing-two-dates-in-cpp/)
- [HACKERRANK - Date and Time](https://www.hackerrank.com/domains/cpp?filters%5Bsubdomains%5D%5B%5D=c-language-features&filters%5Bsubdomains%5D%5B%5D=date-time)
- [Programiz - C++ Date and Time](https://www.programiz.com/cpp-programming/library-function/ctime)

अब आपको पता है कि कैसे दो तारीखों की तुलना करें और इससे कुछ वक्त और सुगमता बचाएं। हमें आशा है कि आपको ये लेख उपयोग