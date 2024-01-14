---
title:    "C: तारीख को स्ट्रिंग में बदलना"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्यों

प्रोग्रामिंग में तारीख को स्ट्रिंग में परिवर्तित करने का मुख्य कारण है कि यह उपयोगकर्ता को अपनी तारीख को विभिन्न तरीकों से प्रदर्शित करने की अनुमति देता है।

## कैसे करें

यहां हम तारीख को स्ट्रिंग में कैसे परिवर्तित कर सकते हैं, उसके लिए कुछ कोड दिए गए हैं।

```C
// स्ट्रिंग का प्रविष्टित करें
char str[6];

// तारीख को प्राप्त करें
int day = 12;
int month = 3;
int year = 2021;

// तारीख को स्ट्रिंग में परिवर्तित करें
sprintf(str, "%d/%d/%d", day, month, year);

// स्ट्रिंग का उत्पादन करें
printf("तारीख: %s", str);

// उत्पादन: तारीख: 12/3/2021
```

## गहराई में जाएं

तारीख को स्ट्रिंग में परिवर्तित करने के लिए, C प्रोग्रामिंग में हम स्प्रिंटफ़ (sprintf) फ़ंक्शन का उपयोग करते हैं। यह स्ट्रिंग को स्वचालित रूप से आकार निर्धारित करता है और उसमें प्रविष्ट की गई मानों के आधार पर उपयुक्त मान को बदलता है। इसके अलावा, हम तिथियों की आवश्यकताओं को सामान्य तरीके से संभालने के लिए कोड में बदलाव कर सकते हैं।

## देखें भी

- [C प्रोग्रामिंग सीखें](https://www.geeksforgeeks.org/c-language-set-1-introduction-to-c/)
- [sprintf फ़ंक्शन की अधिक जानकारी](https://www.tutorialspoint.com/c_standard_library/c_function_sprintf.htm)
- [तारीख में बदलाव करने के लिए अन्य मेथड्स](https://www.includehelp.com/c-programming-questions/how-to-convert-date-to-string-in-c.aspx)