---
title:                "TOML के साथ काम करना"
aliases:
- /hi/vba/working-with-toml/
date:                  2024-02-01T22:07:56.167226-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOML के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/vba/working-with-toml.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

TOML, जिसका अर्थ है Tom's Obvious, Minimal Language, एक डाटा सीरियलाइजेशन प्रारूप है जिसका प्रमुखता से कॉन्फ़िगरेशन फाइलों के लिए उपयोग किया जाता है। प्रोग्रामर इसे इसकी पढ़ने में आसानी और डेटा संरचनाओं के साथ आसान मैपिंग के कारण उपयोग करते हैं, जो विभिन्न प्रोग्रामिंग वातावरणों में, विज़ुअल बेसिक फॉर एप्लीकेशन्स (VBA) सहित, एप्लीकेशन्स की सीधे तौर पर कॉन्फ़िगरेशन सक्षम बनाती है।

## कैसे करें:

VBA में TOML के साथ काम करना शामिल है TOML फाइल को पार्स करना ताकि आपके VBA प्रोजेक्ट में कॉन्फ़िगरेशन या सेटिंग्स को पढ़ा जा सके। VBA में TOML के लिए निर्मित समर्थन नहीं है, इसलिए आमतौर पर आप एक पार्सर का उपयोग करेंगे या TOML डेटा को JSON या XML जैसे फॉर्मेट में बदल देंगे जिसके साथ VBA आसानी से काम कर सकता है। यहां एक सरल TOML कॉन्फ़िग फाइल को मैन्युअली पार्स करने का तरीका बताया गया है:

1. **नमूना TOML फाइल** (`config.toml`):
```
title = "TOML Example"

[database]
server = "192.168.1.1"
ports = [ 8000, 8001, 8002 ]
connection_max = 5000
enabled = true
```

2. **TOML को पार्स करने के लिए VBA कोड**:

मान लीजिए कि TOML सामग्री को एक स्ट्रिंग वेरिएबल `tomlStr` में पढ़ा गया है, निम्नलिखित VBA कोड `[database]` खण्ड को पार्स करने का एक साधारण तरीका दिखाता है:

```vb
Function ParseTOML(tomlStr As String)
    Dim lines() As String
    lines = Split(tomlStr, vbCrLf)
    
    Dim config As Object
    Set config = CreateObject("Scripting.Dictionary")
    Dim currentSection As String
    currentSection = ""
    
    Dim i As Integer
    For i = 0 To UBound(lines)
        Dim line As String
        line = Trim(lines(i))
        If InStr(line, "[") > 0 और InStr(line, "]") > 0 Then
            currentSection = Mid(line, 2, Len(line) - 2)
            Set config(currentSection) = CreateObject("Scripting.Dictionary")
        ElseIf InStr(line, "=") > 0 Then
            Dim parts() As String
            parts = Split(line, "=")
            Dim key As String
            key = Trim(parts(0))
            Dim value As String
            value = Trim(parts(1))
            config(currentSection)(key) = value
        End If
    Next i
    
    'पार्स किए गए डेटा को एक्सेस करने का उदाहरण
    Debug.Print "Database Server: "; config("database")("server")
End Function
```

3. **नमूना आउटपुट** (Immediate Window):
```
Database Server: 192.168.1.1
```

## गहराई से जानिए

डेवलपर समुदाय में TOML की व्यावहारिक स्वीकार्यता अधिक सरल, मानव पठनीय कॉन्फ़िगरेशन फाइलों के प्रति एक प्रवृत्ति को दर्शाती है, जो पहले व्यापक रूप से प्रचलित XML के विपरीत है। TOML के डिजाइन दर्शन ने स्पष्ट सेमांटिक्स पर जोर दिया और न्यूनतम ओवरहेड के साथ सीधे पार्सिंग के लिए लक्ष्य रखा। VBA में TOML को सीधे संभालना मैन्युअल पार्सिंग या बाहरी उपकरणों का उपयोग कर TOML को अधिक VBA-अनुकूल प्रारूप में बदलने में शामिल है क्योंकि मूल समर्थन का अभाव है। हालांकि यह मैन्युअल पार्सिंग विधि एक मौलिक दृष्टिकोण प्रदर्शित करती है, बाहरी पुस्तकालयों या मध्यवर्ती प्रारूपों जैसे JSON का उपयोग अधिक मजबूत और त्रुटि-प्रतिरोधी पार्सिंग रणनीतियों की पेशकश कर सकता है। VBA के Microsoft Office के साथ व्यापक एकीकरण को देखते हुए, TOML को JSON में बदलना और VBA की मूल JSON पार्सिंग क्षमताओं का उपयोग करना (जहाँ लागू हो) या तृतीय-पक्ष JSON पार्सर्स का उपयोग करना एक अधिक सहज कार्यप्रवाह प्रदान कर सकता है। इसके अलावा, डेटा सीरियलाइजेशन प्रारूपों के निरंतर विकास के साथ, प्रोग्रामरों को YAML पर भी विचार करना चाहिए, जो TOML की तरह मानव पठनीयता पर जोर देता है लेकिन जटिलता और लचीलेपन के मामले में विभिन्न ट्रेड-ऑफ़ प्रदान करता है।
