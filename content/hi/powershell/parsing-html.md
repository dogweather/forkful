---
title:                "HTML पार्स करना"
date:                  2024-01-20T15:33:09.285475-07:00
html_title:           "Bash: HTML पार्स करना"
simple_title:         "HTML पार्स करना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/parsing-html.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
HTML पार्स करना मतलब है HTML कोड को समझकर उससे डेटा निकालना। प्रोग्रामर इसे वेबसाइट्स से जरूरी जानकारी एकत्र करने या डेटा माइनिंग के लिए करते हैं।

## कैसे करें:
PowerShell में HTML पार्स करने के लिए, `Invoke-WebRequest` cmdlet का इस्तेमाल करके और `HtmlWebResponseObject` से डेटा एकत्र करने का उदाहरण यहाँ है:

```PowerShell
# वेब पेज प्राप्त करें
$response = Invoke-WebRequest -Uri 'http://example.com'

# सभी लिंक्स एकत्र करें
$links = $response.Links.Href

# पहले 5 लिंक्स दिखाएँ
$links | Select-Object -First 5
```

यह कोड सिर्फ वेब पेज से पहले पांच लिंक्स दिखाएगा।

## गहराई से जानकारी:
HTML पार्सिंग एक पुराना कांसेप्ट है जो वेब स्क्रेपिंग के रूप में मशहूर है। इसके विकल्पों में लाइब्रेरीज जैसे कि HtmlAgilityPack शामिल हैं, जो डॉट नेट में इस्तेमाल होती हैं। PowerShell का `Invoke-WebRequest` एक सरल सहज औज़ार है जो विशिष्ट HTML तत्वों जैसे कि फॉर्म, इमेजेज और लिंक्स तक एक्सेस प्रदान करता है। हालांकि, जटिल HTML डॉक्यूमेंट्स को पार्स करने के लिए ज्यादा शक्तिशाली पार्सर्स की जरूरत हो सकती है।

## संबंधित स्रोत:
- [PowerShell's Web Cmdlets](https://docs.microsoft.com/en-us/powershell/scripting/whats-new/what-s-new-in-powershell-70#web-cmdlets)
- [HtmlAgilityPack GitHub repository](https://github.com/zzzprojects/html-agility-pack)
- [Invoke-WebRequest documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest)
