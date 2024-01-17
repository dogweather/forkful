---
title:                "तारीख को स्ट्रिंग में बदलना"
html_title:           "PowerShell: तारीख को स्ट्रिंग में बदलना"
simple_title:         "तारीख को स्ट्रिंग में बदलना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
किसी भी विकासक को डेट को स्ट्रिंग में बदलने की क्षमता बहुत ही महत्वपूर्ण होती है। यह एक डेट या समय को उचित तारीख और समय स्ट्रिंग में विस्तार से दर्शाता है। उन्हें प्रोग्रामिंग में समय और तारीख के साथ काम करने के लिए उनका उपयोग करना पड़ता है। 

## कैसे
यदि हम 27 दिसंबर 2021 को दिनांक को स्ट्रिंग में बदलना चाहते हैं, तो हमें निम्न रूप में कोड को चलाना होगा:

```PowerShell
Get-Date -date '27 December 2021' -Format 'dddd, dd MMMM yyyy'
```

आपको निम्न प्रकार का परिणाम मिलेगा:

```PowerShell
Monday, 27 December 2021
```

आप इस तरह से एक डेट को किसी भी तारीख फॉर्मेट में स्ट्रिंग में बदल सकते हैं। उदाहरण के लिए, आप 'MM/dd/yyyy' तारीख फॉर्मेट को भी चुन सकते हैं।

## डीप डाइव
यह प्राचीन समय के प्रशंसकों को सुविधा देता है क्योंकि वे कुछ आसानी से मिलाने के लिए कठिन बनानेवाली दिनांकों को पता कर सकते हैं। इसके अलावा, यह उपयोगकर्ताओं को अपने कोड में तारीख और समय को स्ट्रिंग में बदलने का सुविधा प्रदान करता है। अलग बिंदुओं के बीच फॉर्मेट अनुकूलन के भी उपयोगकर्ताओं को अनुमान लगाने की आवश्यकता नहीं होती है।

## इस पर भी देखे
अगर आप पूर्व से प्रोग्रामिंग विषय में अपनी जानकारी बढ़ाना चाहते हैं तो आप निम्न लिंक का उपयोग कर सकते हैं:

- [PowerShell डेट और समय का उपयोग](https://docs.microsoft.com/en-us/powershell/scripting/samples/manipulating-dates-and-times?view=powershell-7.1)
- [सूक्ष्मतम स्तर पर PowerShell समय की प्रस्तुति](https://x-team.com/blog/handling-dates-powershell/)
- [PowerShell फॉरमेट की जाँच](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/format-custom?view=powershell-7.1)