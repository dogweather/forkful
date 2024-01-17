---
title:                "स्ट्रिंग जोड़ना"
html_title:           "Python: स्ट्रिंग जोड़ना"
simple_title:         "स्ट्रिंग जोड़ना"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## क्या है और क्यों करें?
स्ट्रिंग के जोड़ने का मतलब होता है कि हम दो या अधिक स्ट्रिंग्स को एक साथ जोड़ते हैं। प्रोग्रामर्स इसे अक्सर अपने प्रोग्राम्स में उपयोग करते हैं ताकि वे एक ही स्ट्रिंग में अधिक से अधिक डेटा को संग्रहित कर सकें।

## कैसे करें:
```python
# स्ट्रिंग्स को एक साथ जोड़ने के लिए "+"" ऑपरेटर का उपयोग करें
str1 = "नमस्ते"
str2 = "पाइथन"
str3 = str1 + str2
print(str3)

# आप कोई भी संख्या या वेरिएबल को स्ट्रिंग में कन्वर्ट करके भी उपयोग कर सकते हैं
age = 25
message = "मेरी उम्र " + str(age) + " साल है।"
print(message)
```
आउटपुट:
```
नमस्तेपाइथन
मेरी उम्र 25 साल है।
```

## डीप डाइव:
स्ट्रिंग्स को जोड़ने को सॉफ्टवेयर डेवलपमेंट में अक्सर "कॉनकैटेनेशन" के नाम से जाना जाता है। इसका इतिहास बहुत पुराना है और इसे पेंच जोड़ने या चेनिंग भी कहा गया है। इसके अलावा, आप "+" ऑपरेटर के बजाय "join()" और "format()" जैसे फक्शन का भी उपयोग कर सकते हैं। स्ट्रिंग को जोड़ने से पहले, पाइथन इंटरप्रीटर आपको स्वचालित रूप से स्ट्रिंग और अन्य डेटा टाइप को सामान्यतया विकल्प रूप में जोड़ देता है।

## देखें भी:
- [कॉनकैटेनेशन और साम्प्रदायिक बदलाव](https://www.tutorialspoint.com/python/string_concatenation.html)
- [join() और format() फक्शन के बारे में अधिक जानकारी](https://www.geeksforgeeks.org/python-string-concatenation-ordereddict/?ref=rp)
- [पाइथन स्ट्रिंग्स के बारे में अधिक सीखें](https://docs.python.org/3/tutorial/introduction.html#strings)