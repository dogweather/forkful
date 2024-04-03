---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:32.827254-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: VBA \u092E\u0947\u0902\
  , `Dictionary` \u0911\u092C\u094D\u091C\u0947\u0915\u094D\u091F \u0938\u0939\u092F\
  \u094B\u091C\u0940 \u090F\u0930\u0947 \u0915\u0947 \u0938\u092E\u093E\u0928 \u0915\
  \u093E\u0930\u094D\u092F\u0915\u094D\u0937\u092E\u0924\u093E \u092A\u094D\u0930\u0926\
  \u093E\u0928 \u0915\u0930\u0924\u093E \u0939\u0948\u0964 \u0907\u0938\u0915\u093E\
  \ \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\
  \u090F \u0906\u092A\u0915\u094B \u092A\u0939\u0932\u0947 Microsoft Scripting Runtime\
  \ \u0915\u0947\u2026"
lastmod: '2024-03-13T22:44:52.025062-06:00'
model: gpt-4-0125-preview
summary: "VBA \u092E\u0947\u0902, `Dictionary` \u0911\u092C\u094D\u091C\u0947\u0915\
  \u094D\u091F \u0938\u0939\u092F\u094B\u091C\u0940 \u090F\u0930\u0947 \u0915\u0947\
  \ \u0938\u092E\u093E\u0928 \u0915\u093E\u0930\u094D\u092F\u0915\u094D\u0937\u092E\
  \u0924\u093E \u092A\u094D\u0930\u0926\u093E\u0928 \u0915\u0930\u0924\u093E \u0939\
  \u0948\u0964 \u0907\u0938\u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\
  \u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0906\u092A\u0915\u094B \u092A\u0939\
  \u0932\u0947 Microsoft Scripting Runtime \u0915\u0947 \u0932\u093F\u090F \u090F\u0915\
  \ \u0938\u0902\u0926\u0930\u094D\u092D \u091C\u094B\u0921\u093C\u0928\u093E \u0939\
  \u094B\u0917\u093E."
title: "\u0938\u0939\u092F\u094B\u0917\u0940 \u0905\u0930\u0947 \u0915\u093E \u0909\
  \u092A\u092F\u094B\u0917 \u0915\u0930\u0928\u093E"
weight: 15
---

## कैसे करें:
VBA में, `Dictionary` ऑब्जेक्ट सहयोजी एरे के समान कार्यक्षमता प्रदान करता है। इसका उपयोग करने के लिए आपको पहले Microsoft Scripting Runtime के लिए एक संदर्भ जोड़ना होगा:

1. VBA संपादक में, Tools > References... पर जाएं।
2. "Microsoft Scripting Runtime" को चेक करें और OK पर क्लिक करें।

यहाँ `Dictionary` में घोषणा, पॉपुलेशन, और आइटम्स को एक्सेस करने का तरीका दिया गया है:

```vb
Dim sampleDictionary As Dictionary
Set sampleDictionary = New Dictionary

' आइटम्स जोड़ना
sampleDictionary.Add Key:="Name", Item:="John Doe"
sampleDictionary.Add Key:="Age", Item:=29
sampleDictionary.Add Key:="Occupation", Item:="Engineer"

' आइटम्स को एक्सेस करना
Debug.Print sampleDictionary.Item("Name")  ' आउटपुट: John Doe
Debug.Print sampleDictionary.Item("Age")   ' आउटपुट: 29

' यदि कुंजी मौजूद है तो जाँचना
If sampleDictionary.Exists("Occupation") Then
    Debug.Print "Occupation Key Exists"
End If

' आइटम्स हटाना
sampleDictionary.Remove("Occupation")

' डिक्शनरी के माध्यम से लूपिंग
For Each Key In sampleDictionary.Keys
    Debug.Print Key & ": " & sampleDictionary.Item(Key)
Next Key
```

## गहराई में
`Dictionary` ऑब्जेक्ट अंतर्निहित रूप से Windows Scripting Host के घटकों के साथ इंटरफेस करता है। इस प्रकार, यह एक देर से बंधा हुआ COM ऑब्जेक्ट है, जो अतीत में VBA की कार्यक्षमता को बढ़ाने का एक सामान्य तरीका था। VBA में इसका उपयोग जटिल डाटासेट्स को संभालने की भाषा की क्षमता को महत्वपूर्ण रूप से बढ़ा सकता है, बिना पारंपरिक एरे या Excel रेंज में देखे गए कठोर संरचना को लागू किए।

एक सीमा यह है कि `Dictionary` तक पहुँचने के लिए Microsoft Scripting Runtime के लिए एक संदर्भ सेट करना आवश्यक है, जो आपके VBA प्रोजेक्ट्स के वितरण को जटिल बना सकता है। VBA के भीतर Collections जैसे विकल्प मौजूद हैं लेकिन कुछ `Dictionary` के कुंजी विशेषताओं की कमी है, जैसे कि बिना त्रुटि ट्रिगर किए एक कुंजी के अस्तित्व की जांच करने की क्षमता।

अधिक हालिया प्रोग्रामिंग संदर्भों में, Python जैसी भाषाएं बाहरी संदर्भों को जोड़ने की आवश्यकता के बिना सहयोजी एरे (जिन्हें Python में भी शब्दकोश के रूप में जाना जाता है) के लिए अंतर्निहित समर्थन प्रदान करती हैं। यह अंतर्निहित समर्थन प्रक्रिया को सुगम बनाता है और बॉक्स से बाहर अधिक उन्नत सुविधाएँ प्रदान करता है। हालाँकि, VBA के दायरे में और Microsoft Office सुइट में कार्यों को स्वचालित करने के लिए विशिष्ट अनुप्रयोगों के लिए, `Dictionary` ऑब्जेक्ट का उपयोग सहयोजी एरे-जैसे डेटा संरचनाओं के लिए एक शक्तिशाली और प्रासंगिक विधि बनी हुई है।
