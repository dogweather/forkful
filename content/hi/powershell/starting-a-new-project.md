---
title:                "एक नया प्रोजेक्ट शुरू करना"
html_title:           "PowerShell: एक नया प्रोजेक्ट शुरू करना"
simple_title:         "एक नया प्रोजेक्ट शुरू करना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
 
एक नया प्रोजेक्ट शुरू करना क्या है और क्यों प्रोग्रामर इसे करते हैं? एक नए प्रोजेक्ट को शुरू करना एक मूल्यवान कार्य है जो किसी भी विकासक के लिए आवश्यक होता है। यह उनके द्वारा निर्मित सॉफ्टवेयर या वेबसाइट की शुरुआती बनने की आधार पर होता है।

## कैसे करें?
 
```PowerShell 
New-Item -ItemType Directory -Path C:\Users\NewProject
```
ऊपर दिए गए कोड का उपयोग करके आप अपने कंप्यूटर पर एक नया प्रोजेक्ट शुरू कर सकते हैं। यह कमांड आपको नए डायरेक्टरी को बनाने के लिए प्रोम्प्ट करेगी जिसका नाम आप अपनी पसंद के अनुसार दे सकते हैं। आप भी अपने प्रोजेक्ट के लिए एक नया फ़ोल्डर बना सकते हैं और उसमें अपने सभी फ़ाइल और पैकेज्स को संग्रहीत कर सकते हैं।

## गहराई पर जाएं
 
इस प्रकार की स्थितियों में, PowerShell में हर प्रोजेक्ट को शुरू करने का एक सरल और संगठित तरीका है। इससे पहले, कई लोग इसके लिए कमांड लाइन उपयोग करते थे, लेकिन PowerShell आपको इसका एक बेहतर और आसान विकल्प प्रदान करता है। अन्य विकल्पों में शामिल हैं भाषाएं जैसे कि C # और Python हैं जो प्रोजेक्ट शुरू करने के दृष्टिकोण से सिर्फ़ कुछ हद तक PowerShell से मुख्यतः अलग हैं। एक बार आप इस प्रक्रिया को समझ लेंगे, आप किसी भी प्रोजेक्ट को शुरू करने के लिए PowerShell का उपयोग कर सकते हैं।

## और देखें
 
[PowerShell प्रोजेक्ट को शुरू करने के संबंधित स्रोतों के लिए यहाँ क्लिक करें](https://docs.microsoft.com/en-us/powershell/scripting/getting-started/starting-a-new-project?view=powershell-7). इसमें आपको केवल यह नहीं बताया जाएगा कि आप कैसे एक नया प्रोजेक्ट शुरू कर सकते हैं, बल्कि आपको भाषा की प्रविष्टियों और अन्य महत्वपूर्ण कार्यों के बारे में भी बताया जाएगा जो आपको जानने में मदद करेंगे।