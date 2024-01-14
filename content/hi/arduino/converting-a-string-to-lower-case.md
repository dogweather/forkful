---
title:    "Arduino: स्ट्रिंग को लोअर केस में रूपांतरित करना"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## क्यों

कोई भी प्रोग्रामिंग भाषा में स्ट्रिंग को लोअर केस में तबदील करने का काम धीरे-धीरे बहुत ही जरूरी हो गया है। इससे हमारे कोड में निजी और सार्वजनिक चरित्रों को आसानी से पहचाना जा सकता है और कोड को दुर्भाषित करने में सहायता मिलती है।

## कैसे करें

```
Arduino String str = "HELLO";
Serial.println(str.toLowerCase());
```

```
Output: hello
```

स्ट्रिंग को लोअर केस में तबदील करने के लिए हमें स्ट्रिंग वेरिएबल को `toLowerCase()` फंक्शन के साथ पास करना होगा। इससे स्ट्रिंग का प्रथम अक्षर कोलोन घोषणा को छोड़कर पूरी स्ट्रिंग को लोअर केस में तबदील किया जाएगा।

## गहराईवाला गढ़

सामान्य तौर पर, स्ट्रिंग को लोअर केस में तबदील करना एक सरल काम है। परंतु, अगर हमारा स्ट्रिंग ASCII कोडिंग स्टैण्डर्ड को लोअर केस में उपयोग करता है तो उससे समस्याएं उत्पन्न हो सकती हैं। इसलिए, कई बार हमें स्ट्रिंग में उपप्रान्त अक्षरों को भी लोअर केस में बदलना पड़ता है। इसलिए, हमें कोड में इस अंश को भी शामिल करना चाहिए।

## देखें भी

"स्ट्रिंग को अपर केस से लोअर केस में तबदील करना", https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/

"अलग स्ट्रिंग के साथ पहली स्ट्रिंग की मेल करना", https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/charat/

"स्ट्रिंग में विशिष्ट अक्षर ढूंढना", https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/indexof/

"स्ट्रिंग संदर्भ", https://www.arduino.cc/reference/en/language/variables/data-types/string/

अ