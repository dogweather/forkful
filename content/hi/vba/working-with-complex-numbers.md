---
title:                "जटिल संख्याओं के साथ काम करना"
date:                  2024-02-01T22:09:35.338346-07:00
model:                 gpt-4-0125-preview
simple_title:         "जटिल संख्याओं के साथ काम करना"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/vba/working-with-complex-numbers.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

जटिल संख्याओं के साथ काम करना उन संख्याओं पर गणितीय संचालन करने को शामिल करता है जिनमें एक वास्तविक भाग और एक काल्पनिक भाग होता है। प्रोग्रामर अक्सर इंजीनियरिंग, भौतिक विज्ञान, और ऐसे क्षेत्रों में जटिल संख्याओं के साथ काम करते हैं जहाँ केवल वास्तविक संख्याओं के साथ समीकरणों को हल करना संभव नहीं होता।

## कैसे:

विज़ुअल बेसिक फॉर एप्लिकेशंस (VBA) में, जटिल संख्याओं के साथ काम करना, उन भाषाओं की तुलना में कुछ कम सरल हो सकता है जिनका उनके लिए स्वाभाविक समर्थन होता है। हालाँकि, आप फ़ंक्शन बनाकर या मौजूदा लाइब्रेरी फ़ंक्शन का उपयोग करके जटिल संचालनों को संभाल सकते हैं। आइए जटिल संख्याओं के जोड़, घटाव, गुणा, और विभाजन का एक बुनियादी उदाहरण देखें:

```vb
' जटिल संख्याओं को जोड़ने का फंक्शन
Function AddComplex(x As String, y As String) As String
    Dim real1 As Double, imag1 As Double
    Dim real2 As Double, imag2 As Double
    
    ' जटिल संख्याओं से वास्तविक और काल्पनिक भागों को निकालना
    real1 = Val(Split(x, "+")(0))
    imag1 = Val(Split(x, "+")(1))
    real2 = Val(Split(y, "+")(0))
    imag2 = Val(Split(y, "+")(1))
    
    ' जोड़ करना
    AddComplex = (real1 + real2) & "+" & (imag1 + imag2) & "i"
End Function

' उदाहरण उपयोग
Sub ExampleUsage()
    Dim result As String
    result = AddComplex("3+2i", "1+7i")
    Debug.Print "जोड़ का परिणाम: " & result  ' आउटपुट: जोड़ का परिणाम: 4+9i
End Sub
```

जबकि यह जोड़ को प्रदर्शित करता है, घटना, गुणन, और विभाजन के लिए समान दृष्टिकोण को अनुरूप किया जा सकता है। मौलिक अंकगणित से परे जटिल संचालनों के लिए, बाहरी पुस्तकालयों का पता लगाना या अधिक स्वाभाविक रूप से जटिल संख्या संचालन का समर्थन करने वाले अन्य समाधानों को एकीकृत करना उपयोगी हो सकता है।

## गहराई से अध्ययन:

VBA में जटिल संख्याओं के लिए निर्मित समर्थन शामिल नहीं है, यह एक पहलू है जहाँ यह भाषाओं जैसे कि Python, जिसमें एक जटिल संख्या वर्ग होता है (`complex`) या C++ जिसके अपने स्टैंडर्ड टेम्पलेट लाइब्रेरी (`std::complex`) होती है, के पीछे है। ऐतिहासिक रूप से, VBA में सीधे जटिल संख्याओं को संभालने की आवश्यकता अपेक्षाकृत दुर्लभ है, क्योंकि इसका उपयोग अक्सर ऑटोमेशन, ऑफिस एप्लिकेशन को संभालने, और पारंपरिक रूप से जटिल गणितीय गणनाओं की आवश्यकता नहीं होने वाले कार्यों के लिए किया जाता है। जब VBA की कल्पना की गई थी और विकसित की गई थी, इसके उपयोग के मामले मुख्य रूप से वैज्ञानिक कंप्यूटिंग के बजाय व्यावसायिक अनुप्रयोगों पर केंद्रित थे, जो इसके लिए की गई चूक की व्याख्या कर सकते हैं।

जटिल संख्या संचालनों के व्यापक कार्यों की आवश्यकता वाले कार्यों के लिए, प्रोग्रामर को अधिक गणितीय उन्मुख भाषा का उपयोग करना लाभकारी लग सकता है। हालांकि, VBA का उपयोग करने के लिए प्रतिबद्ध या प्रतिबंधित लोगों के लिए, कस्टम फंक्शन लिखना (जैसा कि दिखाया गया है) या उन सॉफ्टवेयर के साथ एकीकरण करना जिनमें ये क्षमताएं हैं (जैसे MATLAB या किसी हद तक Excel खुद) व्यवहार्य मार्ग हैं। इसकी सीमाओं के बावजूद, रचनात्मक समाधान और बाहरी एकीकरण VBA की उपयोगिता को उन क्षेत्रों में विस्तारित कर सकते हैं जिनके लिए यह मूल रूप से डिज़ाइन नहीं किया गया था, जिसमें जटिल संख्याओं के साथ काम करना शामिल है।