---
title:                "डायरेक्टरी मौजूद है या नहीं जांचना"
date:                  2024-02-01T21:50:36.646291-07:00
model:                 gpt-4-0125-preview
simple_title:         "डायरेक्टरी मौजूद है या नहीं जांचना"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/vba/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

Visual Basic for Applications (VBA) में एक निर्देशिका के अस्तित्व की जांच इस बात को सत्यापित करने के बारे में है कि फ़ाइल सिस्टम में कोई फोल्डर मौजूद है या नहीं, इससे पहले कि ऑपरेशन जैसे कि फ़ाइलों को सहेजना या नई निर्देशिकाएँ बनाना किए जाएं। प्रोग्रामर ऐसा करते हैं ताकि रनटाइम त्रुटियों से बचा जा सके और सुनिश्चित किया जा सके कि उनका कोड फ़ाइल सिस्टम के साथ कुशलता और सही तरीके से संवाद करे।

## कैसे करें:

VBA में यदि एक निर्देशिका का अस्तित्व जांचना है, तो आप आम तौर पर `Dir` फ़ंक्शन का उपयोग `vbDirectory` विशेषता के साथ मिलाकर करते हैं। यह दृष्टिकोण आपको इसके पथ को निर्दिष्ट करके किसी फोल्डर के अस्तित्व की जांच करने की अनुमति देता है। आप यह कैसे कर सकते हैं:

```basic
Dim folderPath As String
folderPath = "C:\TestFolder"

If Dir(folderPath, vbDirectory) = "" Then
    MsgBox "निर्देशिका मौजूद नहीं है।", vbExclamation
Else
    MsgBox "निर्देशिका मौजूद है।", vbInformation
End If
```

यह कोड स्निपेट पहले एक फोल्डर पथ (`C:\TestFolder`) को परिभाषित करता है। फिर `Dir` फंक्शन इस फोल्डर को `vbDirectory` विशेषता का उपयोग करके ढूंढता है। यदि फोल्डर मौजूद नहीं है, तो `Dir` एक खाली स्ट्रिंग लौटाएगा, और हम एक संदेश बॉक्स दिखाते हैं जो इंगित करता है कि निर्देशिका मौजूद नहीं है। अन्यथा, हम एक अलग संदेश प्रदर्शित करते हैं जो कहता है कि निर्देशिका मौजूद है।

निर्देशिका मौजूद नहीं होने पर नमूना आउटपुट:
```
निर्देशिका मौजूद नहीं है।
```

निर्देशिका मौजूद होने पर नमूना आउटपुट:
```
निर्देशिका मौजूद है।
```

## गहराई से समझें

एक निर्देशिका के अस्तित्व की जांच कई प्रोग्रामिंग भाषाओं में एक मूल भूमिका है, न केवल VBA में। ऊपर वर्णित `Dir` का उपयोग करने की विधि अधिकांश उद्देश्यों के लिए VBA में सरल और प्रभावी है। हालांकि, इस दृष्टिकोण में सीमाएँ हो सकती हैं, जैसे कि नेटवर्क पथों के मामलों में और अनुमतियों की संभाल में, जो कभी-कभी गलत सकारात्मक या नकारात्मक परिणाम दे सकते हैं। 

ऐतिहासिक रूप से, फाइल सिस्टम एक्सेस विधियां विभिन्न प्रोग्रामिंग भाषाओं में विकसित हुई हैं, जिनमें नए वाले ऑब्जेक्ट-ओरिएंटेड दृष्टिकोण पेश करते हैं। उदाहरण के लिए, VB.NET जैसी .NET भाषाओं में, कोई `System.IO.Directory.Exists(path)` का उपयोग कर सकता है जो निर्देशिका के अस्तित्व की जांच करने के लिए एक और सीधा और कदाचित अधिक शक्तिशाली तरीका प्रदान करता है, अपवाद संभालने और समृद्ध वापसी जानकारी से लाभ उठाता है।

जबकि VBA में .NET के रूप में मजबूत निर्मित कक्षाएं नहीं हैं जो फाइल सिस्टम ऑपरेशनों के लिए पाई जाती हैं, `Dir` फंक्शन की उपयोगिता और सीमाओं को समझना VBA स्क्रिप्ट्स लिखने के लिए महत्वपूर्ण है जो फाइल सिस्टम के साथ बातचीत करती हैं। जहां VBA की क्षमताएं अपर्याप्त हैं, .NET घटकों को एकीकृत करने या बाहरी स्क्रिप्ट का उपयोग करना बेहतर विकल्प प्रदान कर सकता है।