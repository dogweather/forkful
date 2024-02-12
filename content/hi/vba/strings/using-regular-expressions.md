---
title:                "रेगुलर एक्सप्रेशन्स का उपयोग करना"
aliases:
- /hi/vba/using-regular-expressions.md
date:                  2024-02-01T22:06:12.924680-07:00
model:                 gpt-4-0125-preview
simple_title:         "रेगुलर एक्सप्रेशन्स का उपयोग करना"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/vba/using-regular-expressions.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

Visual Basic for Applications (VBA) में नियमित अभिव्यक्तियाँ (regex) शब्दों की खोज, मिलान, और हेरफेर करने का एक शक्तिशाली तरीका प्रदान करती हैं। प्रोग्रामर डेटा मान्यता, पार्सिंग, और रूपांतरण जैसे कार्यों के लिए उनका उपयोग करते हैं क्योंकि जटिल शब्द पैटर्नों को संभालने में उनकी लचीलापन और कुशलता होती है।

## कैसे करें:

VBA में नियमित अभिव्यक्तियों का उपयोग करने के लिए, आपको पहले Microsoft VBScript Regular Expressions लाइब्रेरी को सक्षम करना होगा। VBA एडिटर में, `Tools` -> `References` में जाएं, फिर `Microsoft VBScript Regular Expressions 5.5` की जाँच करें।

यहाँ एक बुनियादी उदाहरण है यह जानने के लिए कि एक पैटर्न शब्द में मौजूद है या नहीं:

```vb
Sub FindPattern()
    Dim regex As Object
    Set regex = CreateObject("VBScript.RegExp")

    With regex
        .Global = True
        .IgnoreCase = True
        .Pattern = "\bis\b"  ' शब्द "is" की तलाश करता है
    End With
    
    Dim testString As String
    testString = "This is a test string."
    
    If regex.Test(testString) Then
        MsgBox "पैटर्न मिला।"
    Else
        MsgBox "पैटर्न नहीं मिला।"
    End If
End Sub
```

एक शब्द में पैटर्न को बदलने के लिए:

```vb
Sub ReplacePattern()
    Dim regex As Object, replacedString As String
    Set regex = CreateObject("VBScript.RegExp")
    
    With regex
        .Global = True
        .IgnoreCase = False
        .Pattern = "\s"  ' किसी भी सफेद स्थान वर्ण से मिलता है
    End With
    
    replacedString = regex.Replace("This is a test string.", "_")
    MsgBox replacedString  ' आउटपुट्स: "This_is_a_test_string."
End Sub
```

## गहराई से डाइव

प्रोग्रामिंग भाषाओं में नियमित अभिव्यक्तियों का समावेश अक्सर 1970 के दशक से यूनिक्स उपकरणों में वापस जाता है। VBA ने VBScript Regular Expressions लाइब्रेरी के माध्यम से regex को एकीकृत किया, यह दर्शाता है कि Excel या Access जैसे भारी टेक्स्ट हेरफेर से आमतौर पर जुड़ी नहीं होती प्रक्रियाओं में भी इसकी महत्वपूर्णता है।

उनकी शक्ति के बावजूद, VBA में regex कभी-कभी Python या JavaScript जैसी आधुनिक भाषाओं की तुलना में कम सहज या प्रदर्शन-सक्षम हो सकता है। उदाहरण के लिए, Python का `re` मॉड्यूल नामित समूहों के लिए व्यापक समर्थन और अधिक सोफ़िस्टिकेटेड पैटर्न-मिलान क्षमताएं प्रदान करता है, जो एक स्वच्छ और संभवतः अधिक पठनीय दृष्टिकोण प्रदान करता है। हालाँकि, VBA पारिस्थितिकी तंत्र के भीतर काम करते समय, नियमित अभिव्यक्तियाँ पैटर्न मिलान या टेक्स्ट हेरफेर करने वाले कार्यों के लिए एक अमूल्य उपकरण बनी रहती हैं। सुविधा और क्षमताओं के दृष्टिकोण से Office अनुप्रयोगों में शब्दों के साथ व्यवहार करते समय कुशलता का व्यापार अक्सर नगण्य होता है।
