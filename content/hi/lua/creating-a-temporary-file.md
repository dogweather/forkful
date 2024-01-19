---
title:                "एक अस्थायी फ़ाइल बनाना"
html_title:           "Arduino: एक अस्थायी फ़ाइल बनाना"
simple_title:         "एक अस्थायी फ़ाइल बनाना"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्या एवं क्यों?
अस्थायी फ़ाइल बनाना का मतलब होता है कि आप कुछ समय के लिए एक फ़ाइल बनाते हैं जो बाद में अपने आप नष्ट हो जाती है। प्रोग्रामर्स कीुछ कामचलाऊ कठिनाईयों को हल करने के लिए इसका उपयोग करते हैं, जैसे कम स्थान के कारण डाटा खोने से बचने या अस्थायी डाटा को संग्रहित करने के लिए।

## कैसे करे:
Lua में आप टेम्पररी फ़ाइल `os.tmpname` फ़ंक्शन का उपयोग करके बना सकते हैं। देखें:

```Lua
tmpname = os.tmpname()
print(tmpname)
```

इस स्क्रिप्ट को चलाने पर आपको एक अस्थायी फ़ाइल का नाम मिलेगा। इसे आप `io.open` फ़ंक्शन के साथ उपयोग कर सकते हैं। फ़ाइल बनाने के बाद, `os.remove` फ़ंक्शन का उपयोग करके आप इसे हटा सकते हैं। 

## गहराई की जाँच:
अस्थायी फ़ाइल बनाना हमेशा से ही गहन और बारीकी वाला काम रहा है। यह न केवल आपके कोड और कंप्यूटर सिस्टम के बीच कुछ डेटा को सुरक्षित रखने में मदद करता है, बल्कि अन्य प्रग्रामर्स को भी समझने में सहायता करता है। ऐसा करने के अन्य तरीके में `io.tmpfile` फ़ंक्शन शामिल है, जो सीधे एक अस्थायी फ़ाइल बना देता है। 

## देखें भी:
[basic function os.tmpname](https://www.lua.org/manual/5.4/manual.html#6.9)
This is to provide more information about the function used to create temporary files.

[io library](https://www.lua.org/manual/5.4/manual.html#6.8) 
Here you will find more information about the file operations using the io library in Lua.

समाप्ति के बजाय, आगे की पढ़ाई का आपको प्रेरित करना हमारी आवश्यकता है। लुआ में और भी बहुत कुछ सीखने के लिए, उपर दिए गए लिंक्स पर जाएँ।