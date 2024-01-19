---
title:                "स्ट्रिंग्स को जोड़ना"
html_title:           "Bash: स्ट्रिंग्स को जोड़ना"
simple_title:         "स्ट्रिंग्स को जोड़ना"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

"Strings को Concatenate करना" इसका मतलब है की दो या दो से अधिक strings को एक साथ जोड़ देना। यह तब किया जाता है जबु programmers को strings को manipulate करके नई string बनाने की आवश्यकता होती है। 

## कैसे करें:

Python में strings को concatenate करने के लिए "+" operator का उपयोग करें।

```Python
# सैंपल कोड 
string1 = "नमस्ते, "
string2 = "दुनिया!"
string3 = string1 + string2
print(string3)
```

Output:
```Python
नमस्ते, दुनिया!
```

## गहराई में जानकारी:

Strings को concatenate करने का तरीका अद्वितीय नहीं है और कई भाषाओं में इसका उपयोग होता है। Python में "+" operator का उपयोग string concatenation के लिए किया जाता है इसका मुख्य कारण यह है कि "+" operator का उपयोग आसानी से समझा जा सकता है और इसका उपयोग शुरुआती programmers के लिए सरल बना देता है। 

लेकिन मुख्य string concatenation के विकल्पों में .join() और f-strings शामिल हैं। .join() तरीका तब उपयोगी होता है जब एक से अधिक strings को concatenate करने की आवश्यकता होती है, और f-strings उपयोगी होते हैं जब string formatting और expressions को combine करने की आवश्यकता होती है। 

## और भी देखें:

Python के official documentation में strings के बारे में और जानने के लिए, आपको इस [लिंक](https://docs.python.org/3/tutorial/introduction.html#strings) पर क्लिक करना चाहिए। f-strings के बारे में और जानने के लिए आपको इस [लिंक](https://docs.python.org/3/tutorial/inputoutput.html#fancier-output-formatting) पर क्लिक करना चाहिए।