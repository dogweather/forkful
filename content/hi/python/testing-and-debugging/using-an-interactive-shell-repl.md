---
title:                "इंटरैक्टिव शेल (REPL) का उपयोग"
aliases:
- /hi/python/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:18:29.769842-07:00
model:                 gpt-4-0125-preview
simple_title:         "इंटरैक्टिव शेल (REPL) का उपयोग"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
REPL, या Read-Eval-Print Loop, एक प्रोग्रामिंग वातावरण है जो उपयोगकर्ता के एकल इनपुट को लेता है, उन्हें निष्पादित करता है, और उपयोगकर्ता को परिणाम लौटाता है। प्रोग्रामर्स इसे त्वरित परीक्षणों, सीखने, डिबगिंग, या उड़ान पर गणना करने के लिए उपयोग करते हैं।

## कैसे करें:
अपने कमांड लाइन में `python` टाइप करके Python के REPL में सीधा प्रवेश करें। एक बार वहाँ, सरल ऑपरेशन या मल्टी-लाइन कोड का परीक्षण करें:

```Python
>>> 1 + 1
2
>>> for i in range(3):
...     print(i)
... 
0
1
2
```

कार्यों के साथ प्रयोग करें और तत्काल प्रतिक्रिया प्राप्त करें:

```Python
>>> def greet(name):
...     return "Hello, " + name + "!"
... 
>>> greet("Alice")
'Hello, Alice!'
```

पुस्तकालयों के साथ खेलें और वास्तविक समय में उनकी विशेषताओं की खोज करें:

```Python
>>> import math
>>> math.sqrt(16)
4.0
```

एक त्वरित `exit()` या `Ctrl+D` (कभी-कभी Windows पर `Ctrl+Z`) के साथ बाहर निकलें।

## गहन विचार
REPL की अवधारणा Python तक अद्वितीय नहीं है; यह Lisp जितनी पुरानी है। कई भाषाएँ कोड के लिए एक हाथों पर दृष्टिकोण के लिए यह तत्काल, इंटरैक्टिव वातावरण प्रदान करती हैं। नेटिव Python शेल के विकल्पों में IPython और Jupyter Notebook शामिल हैं, जो बेहतर इंटरैक्टिविटी, अधिक विशेषताएं, और अन्य उपकरणों के साथ बेहतर एकीकरण प्रदान करते हैं। Python का मानक REPL सरल है, लेकिन यह Python की पूर्ण शक्ति को एम्बेड करता है, जटिल ऑब्जेक्ट्स और मल्टी-थ्रेडेड कार्यक्रमों को संभालता है, हालांकि इसमें ऑटो-कम्पलीशन और सिंटैक्स हाइलाइटिंग जैसी अधिक उन्नत उपकरणों में मौजूद विशेषताएं नहीं हैं।

## देखें भी
- [इंटरप्रीटर पर Python का आधिकारिक दस्तावेज](https://docs.python.org/3/tutorial/interpreter.html)
- [IPython: एक उन्नत Python शेल](https://ipython.org/)
- [Jupyter प्रोजेक्ट](https://jupyter.org/)
