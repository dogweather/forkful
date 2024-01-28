---
title:                "इंटरैक्टिव शेल (REPL) का उपयोग"
date:                  2024-01-26T04:16:17.073010-07:00
model:                 gpt-4-0125-preview
simple_title:         "इंटरैक्टिव शेल (REPL) का उपयोग"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
इंटरएक्टिव शेल या REPL (Read-Eval-Print Loops), आपको उड़ान पर कोड चलाने देते हैं, फ़ंक्शन्स, एल्गोरिदम्स या विचारों के साथ छेड़छाड़ करने की अनुमति देते हैं। ये कोडिंग के स्क्रैच पैड्स होते हैं, त्वरित और गंदे, पूरे डेव वातावरण की स्थापना के बिना।

## कैसे:
Node.js टर्मिनल के माध्यम से सुलभ एक REPL के साथ आता है। इसे खोलें, और आप तैयार हो जाएं। यहां एक स्वाद है:

```javascript
$ node
> let sum = (a, b) => a + b;
अनिश्चित
> sum(5, 10);
15
> .exit
```

सरल है, सही? वेरिएबल्स, फ़ंक्शन्स को परिभाषित करें, या लूप्स चलाएं। जब समाप्त हो जाए, `.exit` आपको वापस असली दुनिया में ले जाता है।

## गहराई से समीक्षा
REPLs 1960 के दशक से आसपास हैं – LISP ने इस अवधारणा का प्रारंभ किया। विचार: प्रोग्रामर को तत्काल प्रतिक्रिया देना। विकल्प? Node.js REPL के अलावा, ब्राउज़र-आधारित कंसोल जैसे कि Chrome DevTools, ऑनलाइन सैंडबॉक्स जैसे कि JSFiddle, या इंटरएक्टिव प्लेग्राउंड्स के साथ पूर्ण IDEs जैसे कि VSCode हैं।

आमतौर पर, REPL वर्कफ्लोज में अंतर्गत कार्य शामिल होते हैं:
1. इनपुट पढ़ना
2. कोड को कम्पाइल और एक्जीक्यूट करना
3. आउटपुट प्रिंट करना
4. वापस लूप में आना

यह एक साधारण फिर भी प्रभावी चक्र है जिसने इंटरएक्टिव कोडिंग पर जबरजस्त प्रभाव डाला है।

## देखें भी
- [Node.js REPL दस्तावेज़](https://nodejs.org/api/repl.html)
- [REPL पर जावास्क्रिप्ट मॉड्यूल्स के लिए मोज़िला का परिचय](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Modules)
- [JSFiddle](https://jsfiddle.net/)
