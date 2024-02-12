---
title:                "पैटर्न मिलने पर संवर्णों को हटाना"
aliases:
- /hi/vba/deleting-characters-matching-a-pattern/
date:                  2024-02-01T21:53:41.962744-07:00
model:                 gpt-4-0125-preview
simple_title:         "पैटर्न मिलने पर संवर्णों को हटाना"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/vba/deleting-characters-matching-a-pattern.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

Visual Basic for Applications (VBA) में एक विशिष्ट पैटर्न से मेल खाने वाले अक्षरों को हटाना शामिल है, जिसमें कुछ मानदंडों को पूरा करने वाले अक्षरों या स्ट्रिंग की पहचान करना और बाद में उन्हें हटाना शामिल है। यह क्रिया डेटा सफाई और फॉर्मेटिंग कार्यों में आम है, जहां स्ट्रिंग्स से अनावश्यक या अवांछित अक्षरों को हटाना डेटा अखंडता बनाए रखने और आगे की डेटा प्रोसेसिंग को सुविधाजनक बनाने के लिए महत्वपूर्ण है।

## कैसे:

VBA में, आप `Replace` फ़ंक्शन या नियमित अभिव्यक्तियों (regular expressions) का उपयोग करके पैटर्न से मेल खाने वाले अक्षरों को हटा सकते हैं। यहाँ दोनों तरीकों के उदाहरण हैं:

### `Replace` फ़ंक्शन का उपयोग कर

`Replace` फ़ंक्शन विशिष्ट अक्षरों या क्रमों को हटाने के लिए सरल है।

```basic
Sub DeleteSpecificChars()
    Dim originalString As String
    originalString = "123-ABC-456-XYZ"
    
    ' हाइफ़ेन्स को हटाना
    Dim resultString As String
    resultString = Replace(originalString, "-", "")
    
    Debug.Print originalString ' पहले: 123-ABC-456-XYZ
    Debug.Print resultString ' बाद में: 123ABC456XYZ
End Sub
```

### नियमित अभिव्यक्तियों का उपयोग कर

अधिक जटिल पैटर्न के लिए, नियमित अभिव्यक्तियाँ एक शक्तिशाली विकल्प प्रदान करती हैं।

पहले, Visual Basic संपादक में Tools > References के माध्यम से Microsoft VBScript Regular Expressions लाइब्रेरी को सक्षम करें।

```basic
Sub DeletePatternChars()
    Dim regEx As Object
    Set regEx = CreateObject("VBScript.RegExp")
    
    Dim strPattern As String
    strPattern = "\d" ' सभी अंकों से मेल खाने वाला पैटर्न
    
    With regEx
        .Global = True
        .IgnoreCase = True
        .Pattern = strPattern
    End With
    
    Dim originalString As String
    originalString = "Remove 123 and 456"
    
    ' मेल खाने वालों को हटाने के लिए Replace विधि का उपयोग करना
    Dim resultString As String
    resultString = regEx.Replace(originalString, "")
    
    Debug.Print originalString ' पहले: Remove 123 and 456
    Debug.Print resultString ' बाद में: Remove  and 
End Sub
```

## गहराई में

ऐतिहासिक रूप से, VBA में पैटर्न मिलान और स्ट्रिंग मैनिपुलेशन कुछ हद तक सीमित रहा है, विशेष रूप से उन आधुनिक प्रोग्रामिंग भाषाओं की तुलना में जो इन कार्यों के लिए व्यापक स्टैंडर्ड लाइब्रेरियों की पेशकश करती हैं। `Replace` फ़ंक्शन सीधे प्रतिस्थापनों के लिए सरल और कुशल है लेकिन अधिक जटिल पैटर्न मिलान के लिए लचीलापन की कमी है। यहीं पर नियमित अभिव्यक्तियाँ (RegEx) प्रवेश करती हैं, पैटर्न मिलान और स्ट्रिंग मैनिपुलेशन के लिए बहुत अधिक समृद्ध वाक्य रचना प्रदान करती हैं। हालांकि, VBA में RegEx के साथ काम करना अतिरिक्त सेटअप की आवश्यकता है, जैसे कि Microsoft VBScript Regular Expressions संदर्भ को सक्षम करना, जो नए उपयोगकर्ताओं के लिए एक बाधा हो सकती है।

इन सीमाओं के बावजूद, VBA में RegEx समर्थन का परिचय एक महत्वपूर्ण कदम आगे था, पाठ प्रोसेसिंग के साथ काम करने वाले प्रोग्रामरों के लिए एक अधिक शक्तिशाली उपकरण प्रदान करता है। अधिक जटिल परिदृश्यों में जहां बिल्ट-इन स्ट्रिंग फ़ंक्शंस कम पड़ जाते हैं, नियमित अभिव्यक्तियाँ एक बहुउद्देश्यीय और शक्तिशाली विकल्प प्रदान करती हैं।

इसे ध्यान में रखते हुए कि उन पर्यावरणों या प्रोजेक्टों के लिए जहां प्रदर्शन महत्वपूर्ण है, बाहरी लाइब्रेरियों का लाभ उठाना या अन्य प्रोग्रामिंग भाषाओं के साथ एकीकरण बेहतर प्रदर्शन और अधिक सुविधाएँ प्रदान कर सकता है। हालांकि, VBA में कई दैनिक कार्यों के लिए, ये मूल विधियाँ एक व्यावहारिक और सुलभ विकल्प बनी हुई हैं।
