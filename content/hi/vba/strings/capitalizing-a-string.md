---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:51:47.939233-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: VBA \u092E\u0947\u0902\
  \ \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u0947 \u092A\u094D\u0930\
  \u0924\u094D\u092F\u0947\u0915 \u0936\u092C\u094D\u0926 \u0915\u094B \u0915\u0948\
  \u092A\u093F\u091F\u0932\u093E\u0907\u091C\u093C \u0915\u0930\u0928\u0947 \u0915\
  \u0947 \u0932\u093F\u090F \u0938\u0940\u0927\u0947 \u0924\u094C\u0930 \u092A\u0930\
  \ \u090F\u0915 \u092C\u093F\u0932\u094D\u091F-\u0907\u0928 \u092B\u0902\u0915\u094D\
  \u0936\u0928 \u0909\u092A\u0932\u092C\u094D\u0927 \u0928\u0939\u0940\u0902 \u0939\
  \u0948, \u091C\u0948\u0938\u093E \u0915\u093F \u0915\u0941\u091B \u0905\u0928\u094D\
  \u092F \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u093F\u0902\u0917\
  \ \u092D\u093E\u0937\u093E\u090F\u0902\u2026"
lastmod: '2024-04-05T21:53:54.006429-06:00'
model: gpt-4-0125-preview
summary: "VBA \u092E\u0947\u0902 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\
  \ \u0915\u0947 \u092A\u094D\u0930\u0924\u094D\u092F\u0947\u0915 \u0936\u092C\u094D\
  \u0926 \u0915\u094B \u0915\u0948\u092A\u093F\u091F\u0932\u093E\u0907\u091C\u093C\
  \ \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0938\u0940\u0927\u0947\
  \ \u0924\u094C\u0930 \u092A\u0930 \u090F\u0915 \u092C\u093F\u0932\u094D\u091F-\u0907\
  \u0928 \u092B\u0902\u0915\u094D\u0936\u0928 \u0909\u092A\u0932\u092C\u094D\u0927\
  \ \u0928\u0939\u0940\u0902 \u0939\u0948, \u091C\u0948\u0938\u093E \u0915\u093F \u0915\
  \u0941\u091B \u0905\u0928\u094D\u092F \u092A\u094D\u0930\u094B\u0917\u094D\u0930\
  \u093E\u092E\u093F\u0902\u0917 \u092D\u093E\u0937\u093E\u090F\u0902 \u0915\u0930\
  \u0924\u0940 \u0939\u0948\u0902\u0964 \u0939\u093E\u0932\u093E\u0902\u0915\u093F\
  , \u0906\u092A `UCase`, `LCase`, \u0914\u0930 `Mid` \u091C\u0948\u0938\u0947 \u0915\
  \u0941\u091B \u0935\u093F\u0927\u093F\u092F\u094B\u0902 \u0914\u0930 \u092B\u0902\
  \u0915\u094D\u0936\u0928\u094D\u0938 \u0915\u094B \u092E\u093F\u0932\u093E\u0915\
  \u0930 \u092F\u0939 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964\
  \ \u092F\u0939\u093E\u0901 \u090F\u0915 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\
  \u0917 \u0915\u094B \u0915\u0948\u092A\u093F\u091F\u0932\u093E\u0907\u091C\u093C\
  \ \u0915\u0930\u0928\u0947 \u0915\u093E \u090F\u0915 \u0938\u0940\u0927\u093E \u0909\
  \u0926\u093E\u0939\u0930\u0923 \u0939\u0948."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u0915\u0948\
  \u092A\u093F\u091F\u0932\u093E\u0907\u091C \u0915\u0930\u0928\u093E"
weight: 2
---

## कैसे करें:
VBA में स्ट्रिंग के प्रत्येक शब्द को कैपिटलाइज़ करने के लिए सीधे तौर पर एक बिल्ट-इन फंक्शन उपलब्ध नहीं है, जैसा कि कुछ अन्य प्रोग्रामिंग भाषाएं करती हैं। हालांकि, आप `UCase`, `LCase`, और `Mid` जैसे कुछ विधियों और फंक्शन्स को मिलाकर यह कर सकते हैं।

यहाँ एक स्ट्रिंग को कैपिटलाइज़ करने का एक सीधा उदाहरण है:

```vb
Function CapitalizeString(inputString As String) As String
    Dim words As Variant
    words = Split(inputString, " ")
    For i = LBound(words) To UBound(words)
        If Len(words(i)) > 0 Then
            words(i) = UCase(Left(words(i), 1)) & LCase(Mid(words(i), 2))
        End If
    Next i
    CapitalizeString = Join(words, " ")
End Function

Sub ExampleUsage()
    Dim exampleString As String
    exampleString = "hello world from VBA!"
    MsgBox CapitalizeString(exampleString) 'आउटपुट: "Hello World From Vba!"
End Sub
```

`CapitalizeString` फंक्शन इनपुट स्ट्रिंग को शब्दों में विभाजित करता है, प्रत्येक शब्द के पहले अक्षर को कैपिटलाइज़ करता है, और अंत में उन्हें दोबारा एक साथ जोड़कर उचित रूप से कैपिटलाइज़ किया गया स्ट्रिंग बनाता है।

## गहराई से विश्लेषण
Visual Basic for Applications, जो कि 90 के दशक की शुरुआत में Microsoft Office एप्लिकेशन्स के लिए एक मैक्रो भाषा के रूप में उभरा, एक सुलभ प्रोग्रामिंग मॉडल प्रदान करने के लिए डिज़ाइन किया गया था। इसकी स्ट्रिंग मैनिपुलेशन क्षमताएँ, जबकि व्यापक हैं, नई भाषाओं में पाए जाने वाले कुछ उच्च-स्तरीय अमूर्तताओं की कमी है। कई आधुनिक प्रोग्रामिंग वातावरण स्ट्रिंग कैपिटलाइज़ेशन के लिए एक समर्पित विधि प्रदान करते हैं, जिसे अक्सर टाइटल केसिंग या इसी तरह कहा जाता है। उदाहरण के लिए, Python में स्ट्रिंग्स के लिए `.title()` विधि शामिल है।

तुलना करते समय, VBA में स्ट्रिंग शब्दों को कैपिटलाइज़ करने के लिए एकल, बिल्ट-इन फंक्शन की अनुपस्थिति एक नुकसान की तरह प्रतीत हो सकती है। हालांकि, यह प्रोग्रामरों को पाठ को कैसे मैनिपुलेट करने पर गहरी समझ और नियंत्रण प्रदान करता है और विशेष रूप से एक सामान्य विधि द्वारा सख्ती से पालन नहीं किए जाने वाले विवरणों को समायोजित कर सकता है। उदाहरण के लिए, एक्रोनिम्स को संभालना या ऐसे विशेष मामले जहाँ शीर्षकों में कुछ छोटे शब्दों को कैपिटलाइज़ नहीं किया जाना चाहिए, VBA में स्पष्ट फंक्शन्स के माध्यम से बेहतर ढंग से अनुकूलित किया जा सकता है।

फिर भी, जहाँ टेक्स्ट प्रोसेसिंग की मांगें उच्च और विविध होती हैं, बिल्ट-इन स्ट्रिंग मैनिपुलेशन लाइब्रेरीज़ के साथ भाषाएँ एक अधिक कुशल मार्ग प्रदान कर सकती हैं। यह उन परिदृश्यों में है कि VBA के साथ अन्य प्रोग्रामिंग संसाधनों को एकीकृत करना या पूरक करना, या पूरी तरह से दूसरी भाषा चुनना, लाभदायक साबित हो सकता है।
