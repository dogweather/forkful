---
title:                "HTML पार्स करना"
html_title:           "C++: HTML पार्स करना"
simple_title:         "HTML पार्स करना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
HTML पार्सिंग का मतलब होता है एक HTML डॉक्यूमेंट को विश्लेषण करना। प्रोग्रामर्स यह तभी करते हैं जब वे एक HTML डॉक्यूमेंट से डाटा निकालना चाहते हैं।

## कैसे करें:
Gleam में HTML पार्सिंग एकल इस तरह की स्क्रिप्ट द्वारा की जा सकती है:

```Gleam
pub fn main() {
import gleam/httpc
import gleam/html

let document = httpc.get("http://example.com").unwrap()
let parsed = html.parse(document.body).unwrap()

parsed
}
```
इसे चलाने पर, आपको पार्स किए गए HTML डॉक्यूमेंट की एक संरचना मिलेगी।

## गहराई में:
### ऐतिहासिक संदर्भ
HTML पार्सिंग का विचार पहली बार 1990 में WWW project के दौरान किया गया था।

### वैकल्पिक विधियाँ
हालांकि Gleam एक मजबूत विकल्प है, फिर भी अन्य भाषाओं जैसे कि Python, Java और Ruby में भी HTML पार्सिंग लाइब्रेरी मौजूद हैं।

### कार्यान्वयन विवरण
एक HTML पार्सर का उद्देश्य है एक HTML डॉक्यूमेंट को एक संरचनात्मक रूप में बदलना, जिसे DOM (Document Object Model) कहा जाता है।

## अन्य पढ़ने के लिए:
3. Python और अन्य भाषाओं में HTML पार्सिंग: [https://www.crummy.com/software/BeautifulSoup/bs4/doc/](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)