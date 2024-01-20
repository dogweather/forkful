---
title:                "HTML पार्स करना"
html_title:           "C++: HTML पार्स करना"
simple_title:         "HTML पार्स करना"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

HTML पार्सिंग, हमारे कंप्यूटर को वेब पेज की संरचना को पढ़ने और समझने की कला है। कार्यक्रमकर्ता इसे ताकि वे वेबसाइट्स की जानकारी बाहर निकाल सकें और उसे अन्य उद्देश्यों के लिए उपयोग कर सकें, करते हैं।

## कैसे करें:

```C++
// गम्भीर पुस्तकालय लोड करें।
#include <htmlcxx/html/ParserDom.h>

int main() {
 // पार्सर बनायें।
 htmlcxx::HTML::ParserDom parser;

 // HTML कंटेंट का उदाहरण।
 std::string html = "<html><body><h1>नमस्ते दुनिया</h1></body></html>";
  
 // पार्स HTML।
 auto dom = parser.parseTree(html);
  
 // ट्री में प्रवेश करें।
 auto root = dom.begin();

 // रूट से शीर्षक तक।
 auto title = root->find(htmlcxx::HTML::TAG::H1);

 // शीर्षक प्रिंट करें।
 std::cout << title->text() << std::endl;
  
 return 0;
}
```

इसके आउटपुट में "नमस्ते दुनिया" होगा।

## Deep Dive:

1. HTML पार्सिंग का इतिहास:
   HTML पार्सिंग की जरूरत पहली बार WWW (World Wide Web) के विस्तार के साथ आई। यह नए प्रौद्योगिकी से प्रभावित होता है और साथ ही साथ बदलता रहता है।

2. वैकल्पिक विधियाँ:
   BeautifulSoup, lxml और PyQuery जैसे पायथन के पुस्तकालय भी HTML पार्सिंग करने के लिए मौजूद हैं। यह आपके प्रोजेक्ट की आवश्यकताओं पर निर्भर करता है कि आप किसे चुनते हैं।

3. आवश्यक विवरण:
   HTML पार्सर, वेब पृष्ठ के डॉम(DOM) ट्री का निर्माण करता है, जिससे कोड वेबसाइट के विभिन्न हिस्सों को पहुंच सकता है।

## चर्चा वर्जित:

1. [W3Schools HTML Parsing](https://www.w3schools.com/php/php_ref_simplexml.asp)
2. [HTML Parsing in Python](https://docs.python.org/3/library/html.parser.html)
3. [Boost Library for HTML Parsing in C++](https://www.boost.org/doc/libs/1_74_0/libs/beast/doc/html/beast/using_http/basics.html)