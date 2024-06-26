---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:00:23.566034-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: VBA \u092E\u0947\u0902\
  \ \u090F\u0915 \u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093C\u093E\u0907\
  \u0932 \u0915\u094B \u092A\u0922\u093C\u0928\u0947 \u0915\u093E \u0938\u092C\u0938\
  \u0947 \u0938\u0930\u0932 \u0924\u0930\u0940\u0915\u093E `Open` \u0938\u094D\u091F\
  \u0947\u091F\u092E\u0947\u0902\u091F \u0915\u093E \u0909\u092A\u092F\u094B\u0917\
  \ \u0915\u0930\u0928\u093E \u0939\u0948 \u091C\u093F\u0938\u0947 `Input` \u092F\u093E\
  \ `Line Input` \u092B\u093C\u0902\u0915\u094D\u0936\u0902\u0938 \u0915\u0947 \u0938\
  \u093E\u0925 \u0938\u0902\u092F\u094B\u091C\u093F\u0924 \u0915\u093F\u092F\u093E\
  \u2026"
lastmod: '2024-03-13T22:44:52.069988-06:00'
model: gpt-4-0125-preview
summary: "VBA \u092E\u0947\u0902 \u090F\u0915 \u091F\u0947\u0915\u094D\u0938\u094D\
  \u091F \u092B\u093C\u093E\u0907\u0932 \u0915\u094B \u092A\u0922\u093C\u0928\u0947\
  \ \u0915\u093E \u0938\u092C\u0938\u0947 \u0938\u0930\u0932 \u0924\u0930\u0940\u0915\
  \u093E `Open` \u0938\u094D\u091F\u0947\u091F\u092E\u0947\u0902\u091F \u0915\u093E\
  \ \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0928\u093E \u0939\u0948 \u091C\u093F\
  \u0938\u0947 `Input` \u092F\u093E `Line Input` \u092B\u093C\u0902\u0915\u094D\u0936\
  \u0902\u0938 \u0915\u0947 \u0938\u093E\u0925 \u0938\u0902\u092F\u094B\u091C\u093F\
  \u0924 \u0915\u093F\u092F\u093E \u0917\u092F\u093E \u0939\u0948\u0964 \u0906\u092A\
  \ \u0910\u0938\u093E \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902."
title: "\u090F\u0915 \u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093C\u093E\
  \u0907\u0932 \u092A\u0922\u093C\u0928\u093E"
weight: 22
---

## कैसे करें:
VBA में एक टेक्स्ट फ़ाइल को पढ़ने का सबसे सरल तरीका `Open` स्टेटमेंट का उपयोग करना है जिसे `Input` या `Line Input` फ़ंक्शंस के साथ संयोजित किया गया है। आप ऐसा कर सकते हैं:

1. **फ़ाइल को पढ़ने के लिए खोलें** - पहले, आपको फ़ाइल खोलनी होगी। सुनिश्चित करें कि फ़ाइल पथ एप्लिकेशन के लिए सुलभ है।

```basic
Open "C:\example.txt" For Input As #1
```

2. **फ़ाइल सामग्री पढ़ें** - आप `Line Input` का उपयोग करके पंक्ति-दर-पंक्ति या `Input` का उपयोग करके पूरी फ़ाइल को एक बार में पढ़ सकते हैं।

- **पंक्ति-दर-पंक्ति पढ़ना:**

```basic
Dim fileContent As String
While Not EOF(1) ' EOF = फ़ाइल का अंत
    Line Input #1, fileContent
    Debug.Print fileContent ' तत्काल विंडो में पंक्ति को आउटपुट करता है
Wend
Close #1
```

- **पूरी फ़ाइल एक बार में पढ़ना:**

```basic
Dim fileContent As String
Dim fileSize As Long
fileSize = LOF(1) ' LOF = फ़ाइल की लंबाई
If fileSize > 0 Then
    fileContent = Input(fileSize, #1)
    Debug.Print fileContent
End If
Close #1
```

3. **नमूना आउटपुट**:

मान लीजिए `example.txt` में शामिल है:

```
Hello,
This is a sample text file.
Enjoy reading!
```

तत्काल विंडो में आउटपुट पूरे टेक्स्ट या पंक्ति-दर-पंक्ति आधारित होगा जिस विधि को आप चुनते हैं।

## गहन अध्ययन
दशकों से VBA में टेक्स्ट फ़ाइलों को पढ़ना ऑफिस ऑटोमेशन कार्यों का एक मुख्य स्तम्भ रहा है। प्रदर्शित विधियाँ, हालाँकि VBA इकोसिस्टम के भीतर कुशल हैं, आधुनिक प्रोग्रामिंग प्रथाओं की तुलना में पुरानी प्रतीत हो सकती हैं जो अक्सर फ़ाइल ऑपरेशंस के लिए उच्च-स्तरीय अमूर्तीकरण या लाइब्रेरीज़ का उपयोग करती हैं। उदाहरण के लिए, Python `open()` फ़ंक्शन का उपयोग `with` स्टेटमेंट के भीतर करता है, साफ सिंटैक्स और स्वत: फ़ाइल हैंडलिंग क्षमताओं प्रदान करता है।

यह कहने के बावजूद, माइक्रोसॉफ्ट ऑफिस परिवेश की सीमाओं के भीतर काम करते समय, VBA ऑफिस उत्पादों के साथ इंटरऑपरेबिलिटी की आवश्यकता वाले एप्लिकेशनों के लिए फ़ाइलों को मैनिपुलेट करने का एक सीधा और मूल तरीका प्रदान करता है। बिना बाहरी लाइब्रेरीज़ या जटिल कॉन्फ़िगरेशन की आवश्यकता के, एक टेक्स्ट फ़ाइल को खोलने, पढ़ने, और उसकी सामग्री को पंक्ति-दर-पंक्ति या पूरी तरह से प्रोसेस करने की सादगी, VBA को ऑफिस डेवलपर के टूलकिट में एक मूल्यवान उपकरण बनाती है।

अधिक कुशलता से और कम कोड के साथ फ़ाइलों को हैंडलिंग करने के लिए आधुनिक प्रोग्रामिंग भाषाओं में बेहतर विकल्प हैं, फिर भी VBA की क्षमताओं को समझना और उपयोग करना ऑफिस-आधारित एप्लिकेशनों की कार्यक्षमता को बढ़ा सकता है और उत्पादकता को काफी बढ़ा सकता है।
