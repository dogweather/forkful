---
title:                "एक वेब पेज डाउनलोड करना"
html_title:           "Kotlin: एक वेब पेज डाउनलोड करना"
simple_title:         "एक वेब पेज डाउनलोड करना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

वेब पेज को डाउनलोड करना का मतलब होता है कि आप इंटरनेट से उस पेज की कॉपी अपने कंप्यूटर पर लेते हैं। प्रोग्रामर्स इसे डाटा का विश्लेषण करने, वेब क्रॉलिंग और सामग्री को बाद में पढ़ने के लिए करते हैं।

## कैसे करें:

PowerShell का उपयोग करके आप इसे बहुत ही आसानी से कर सकते हैं। यहां कैसे:

```PowerShell
# प्रयोगिका वेब पेज डाउनलोड करने के लिए
$page = Invoke-WebRequest -Uri https://example.com
# सामग्री प्रिंट करें
$page.Content
```

जो आउटपुट होगा वह कुछ इस तरह होगा:

```PowerShell
<!doctype html>
<html>
<head>
    ...
</head>
<body>
    ...
</body>
</html>
```

## गहराई से जानकारी

जब इंटरनेट नई-नई आई थी, तो वेब पेजों को डाउनलोड करने के लिए `wget` और `curl` जैसे टूल्स का उपयोग किया जाता था। PowerShell, .NET Framework पर बना, इसे बहुत आसान बनाता है। `Invoke-WebRequest` कमांडलेट का उपयोग करने के वैकल्पिक तरीके भी हैं, जैसे WebClient और HttpRequest classes। 

जब `Invoke-WebRequest` चलता है, तो यह HTTP GET अनुरोध भेजता है, सर्वर से प्रतिक्रिया प्राप्त करता है, और इसे एक विशेष क्लास, WebResponse के रूप में संग्रहीत करता है, जिसे आप बाद में कंटेंट प्राप्त करने के लिए उपयोग कर सकते हैं।

## अन्य जानकारी स्रोत

1. [PowerShell स्क्रिप्टिंग गाइड](https://powershell.org/) - बहुत अच्छा स्रोत PowerShell सीखने के लिए। 

2. [वेब स्क्रैपिंग गाइड](https://www.datacamp.com/community/tutorials/web-scraping-tutorial-python) - एक उपयोगी गाइड वेब स्क्रैपिंग के बारे में अधिक जानकारी प्राप्त करने के लिए। 

3. [वेब रिक्वेस्ट संकेतांक](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7.1) - `Invoke-WebRequest` कमांडलेट का विस्तृत विवरण।