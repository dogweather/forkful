---
title:                "HTML पार्स करना"
html_title:           "C++: HTML पार्स करना"
simple_title:         "HTML पार्स करना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/parsing-html.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

HTML Parsing सिंधांत रूप से, एक HTML दस्तावेज़ को विश्लेषण करने की प्रक्रिया है, ताकि उसकी संरचना और विषय-वस्तु का पता चल सके। प्रोग्रामर्स इसे वेबसाइटों की ताजगी और विशेषज्ञता में सुधार करने के लिए इस्तेमाल करते हैं। 

## कैसे करें:

PowerShell के साथ HTML पर्सिंग के लिए, आप `Invoke-WebRequest` कमांडलेट का उपयोग कर सकते हैं। यहाँ एक उदाहरण है:

```PowerShell
# वेबसाइट से HTML प्राप्त करें
$response = Invoke-WebRequest -Uri 'https://www.example.com'

# HTML पार्स करें
$parsedHtml = $response.ParsedHtml

# शीर्षक एलिमेंट का पाठ प्राप्त करें
$title = $parsedHtml.getElementsByTagName('title')[0].innerText
```
आउटपुट:

```PowerShell
PS> $title
"Example Domain"
```

## गहरा डाइव

HTML पार्सिंग का इस्तेमाल 90 के दशक में हुआ था। PowerShell उपकरण मे इसे जोड़ा गया। दूसरे विकल्प में Python का BeautifulSoup library और Node.js का Cheerio library शामिल हैं। HTML पार्सिंग का कार्य पहले टैग पर ध्यान केंद्रित करने, फिर उसकी विशेषताओं पर जाने और अंत में उसके भीतर की सामग्री पर निर्भर करता है।

## और भी देखें

1. [Invoke-WebRequest](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest) के बारे में अधिक जानकारी के लिए Microsoft का डाक्युमेंटेशन देखें।
2. Python के लिए [BeautifulSoup](https://www.crummy.com/software/BeautifulSoup/bs4/doc/) library. 
3. Node.js के लिए [Cheerio](https://cheerio.js.org/) library.