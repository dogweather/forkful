---
title:                "पार्सिंग HTML"
html_title:           "C++: पार्सिंग HTML"
simple_title:         "पार्सिंग HTML"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

पार्सिंग HTML क्या है, और इसे क्यों प्रोग्रामर्स करते हैं? पार्सिंग HTML का मतलब है कि आप किसी वेब पेज को ब्राउज़र में देखने के लिए इस्तेमाल होने वाले HTML कोड को समझ रहे हैं। प्रोग्रामर्स इसे करते हैं क्योंकि वे वेब डेवलपमेंट, डेटा स्क्रैपिंग, और डेटा व्यवस्थापन से जुड़ी कई चुनौतियों को हल कर सकते हैं।

## कैसे करें:

```C++
#include <iostream>
#include <string>
#include <algorithm>

using namespace std;

int main() {
  // HTML के कोड को string में स्टोर करें
  string html = "<p>Hello, world!</p>";

  // string में से टैग हटाएं
  html.erase(remove(html.begin(), html.end(), '<'), html.end());
  html.erase(remove(html.begin(), html.end(), '>'), html.end());

  // शब्दों को अलग अलग चिह्नों पर स्प्लिट करें
  vector<string> words;
  stringstream ss(html);
  string word;
  while (ss >> word) {
    words.push_back(word);
  }

  // output: Hello, world!
  cout << words[1] << " " << words[2] << " " << words[3];

  return 0;
}
```

## गहराई में जाएं:

1. इतिहासिक पृष्ठभूमि: HTML के साथ पार्सिंग का शुरुआती रूप से चन्द तरीकों का उल्लेख है, जैसे SGML और XML
2. वैकल्पिक तरीके: कुछ प्रोग्रामर्स दूसरी भाषाओं का उपयोग करके HTML को पार्स करते हैं, जैसे Python के लिए BeautifulSoup और Java के लिए Jsoup
3. प्रयोग करने की स्थिति: इस अनुप्रयोग को में बहुत से तरीके हो सकते हैं, जैसे कि कैसे बिजली का संचार करें, या अपने स्क्रिप्ट से कुछ डेटा स्क्रैप करें

## जाने के लिए:

- [MDN - HTML introduction](https://developer.mozilla.org/en-US/docs/Learn/HTML/Introduction_to_HTML)
- [W3Schools - HTML tutorial](https://www.w3schools.com/html/default.asp)
- [Codecademy - HTML course](https://www.codecademy.com/learn/learn-html)