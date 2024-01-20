---
title:                "HTML पार्स करना"
date:                  2024-01-20T15:31:42.233027-07:00
html_title:           "Bash: HTML पार्स करना"
simple_title:         "HTML पार्स करना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
HTML पार्सिंग, यह एक प्रक्रिया है जिससे हम HTML डॉक्यूमेंट को पढ़-समझकर उसकी संरचना को कोड के द्वारा उपयोग कर पाते हैं। प्रोग्रामर इसे डाटा निकालने, वेब स्क्रैपिंग, और ऑटोमेटेड टेस्टिंग के लिए करते हैं।

## How to: (कैसे करें:)
```gleam
// Gleam के लिए अभी कोई स्टैण्डर्ड HTML पार्सिंग लाइब्रेरी उपलब्ध नहीं है।
// यह उदाहरण एक सामान्य Gleam प्रोग्राम को दर्शाता है।

fn main() {
  let html_content = "<h1>नमस्ते, Gleam से पार्स HTML!</h1>"

  // यहाँ HTML पार्सिंग लॉजिक होगा, जिसे आपको कस्टम बनाना पड़ेगा
  // या एक्स्टर्नल crate/import का उपयोग करना पड़ेगा।

  let parsed_content = "Parsed HTML here" // पार्स्ड HTML इधर होगा

  println!(parsed_content)
}

// आउटपुट
"Parsed HTML here"
```
याद रहे, क्योंकि Gleam HTML पार्सिंग लाइब्रेरी नहीं प्रदान करता, आपको अतिरिक्त टूल या फ्रेमवर्क की जरुरत पड़ सकती है।

## Deep Dive (गहराई से जानकारी)
HTML पार्सिंग का इतिहास XML पार्सिंग के साथ मिल के चलता है, क्योंकि HTML की संरचना XML से प्रेरित है। लेकिन, HTML की लचीलापन की वजह से पार्सर्स को अधिक जटिल बनाया गया। अन्य भाषाओं में जैसे Python में BeautifulSoup या Node.js में cheerio जैसी लाइब्रेरीज मौजूद हैं। Gleam अभी नया है, इसलिए इसमें इस तरह की विकल्प कम हैं। Gleam में पार्सिंग काफी हद तक Rust की पार्सिंग लाइब्रेरीज पर निर्भर करती है, जैसे html5ever का उपयोग करके Rust के Foreign Function Interface (FFI) के माध्यम से।

## See Also (यह भी देखें)
- Rust's html5ever library: [https://github.com/servo/html5ever](https://github.com/servo/html5ever)
- BeautifulSoup for Python: [https://www.crummy.com/software/BeautifulSoup/](https://www.crummy.com/software/BeautifulSoup/)
- cheerio for Node.js: [https://cheerio.js.org/](https://cheerio.js.org/)

इन संसाधनों के माध्यम से आप अपने Gleam प्रोजेक्ट्स के लिए HTML पार्सिंग के और विकल्प ढूँढ सकते हैं।