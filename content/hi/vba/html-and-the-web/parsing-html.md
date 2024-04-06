---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:59:01.767099-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: VBA \u092E\u0947\u0902\
  , \u0906\u092A `Microsoft HTML Object Library` \u0915\u093E \u0909\u092A\u092F\u094B\
  \u0917 \u0915\u0930\u0915\u0947 HTML \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\
  \ \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964 \u0905\u092A\u0928\u0947 VBA\
  \ \u0938\u0902\u092A\u093E\u0926\u0915 \u092E\u0947\u0902 \u0907\u0938 \u0932\u093E\
  \u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u0915\u0947 \u0932\u093F\u090F \u090F\
  \u0915 \u0938\u0902\u0926\u0930\u094D\u092D \u091C\u094B\u0921\u093C\u0947\u0902\
  ,\u2026"
lastmod: '2024-04-05T21:53:54.039331-06:00'
model: gpt-4-0125-preview
summary: "VBA \u092E\u0947\u0902, \u0906\u092A `Microsoft HTML Object Library` \u0915\
  \u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947 HTML \u092A\u093E\
  \u0930\u094D\u0938 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964\
  \ \u0905\u092A\u0928\u0947 VBA \u0938\u0902\u092A\u093E\u0926\u0915 \u092E\u0947\
  \u0902 \u0907\u0938 \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u0915\
  \u0947 \u0932\u093F\u090F \u090F\u0915 \u0938\u0902\u0926\u0930\u094D\u092D \u091C\
  \u094B\u0921\u093C\u0947\u0902, Tools > References \u092A\u0930 \u091C\u093E\u0915\
  \u0930 \u0914\u0930 `Microsoft HTML Object Library` \u0915\u094B \u091A\u0947\u0915\
  \ \u0915\u0930\u0947\u0902\u0964 \u0907\u0938\u0938\u0947 \u0906\u092A\u0915\u094B\
  \ HTML \u0926\u0938\u094D\u0924\u093E\u0935\u0947\u091C\u093C\u094B\u0902 \u0915\
  \u094B \u0928\u0947\u0935\u093F\u0917\u0947\u091F \u0914\u0930 \u092E\u0948\u0928\
  \u093F\u092A\u094D\u092F\u0941\u0932\u0947\u091F \u0915\u0930\u0928\u0947 \u0915\
  \u0947 \u0932\u093F\u090F \u0915\u0915\u094D\u0937\u093E\u0913\u0902 \u0924\u0915\
  \ \u092A\u0939\u0941\u0902\u091A \u092E\u093F\u0932\u0924\u0940 \u0939\u0948\u0964\
  \ \u092F\u0939\u093E\u0901 \u090F\u0915 \u0938\u0930\u0932 \u0909\u0926\u093E\u0939\
  \u0930\u0923 \u0939\u0948 \u091C\u094B \u0926\u093F\u0916\u093E\u0924\u093E \u0939\
  \u0948 \u0915\u093F \u0915\u0948\u0938\u0947 \u090F\u0915 \u092B\u093E\u0907\u0932\
  \ \u0938\u0947 HTML \u0926\u0938\u094D\u0924\u093E\u0935\u0947\u091C\u093C \u0932\
  \u094B\u0921 \u0915\u093F\u092F\u093E \u091C\u093E\u0924\u093E \u0939\u0948 \u0914\
  \u0930 \u0938\u092D\u0940 \u0932\u093F\u0902\u0915\u094D\u0938 (\u090F\u0902\u0915\
  \u0930 \u091F\u0948\u0917\u094D\u0938) \u0915\u094B \u0928\u093F\u0915\u093E\u0932\
  \u093E \u091C\u093E\u0924\u093E \u0939\u0948."
title: "HTML \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\u0928\u093E"
weight: 43
---

## कैसे करें:
VBA में, आप `Microsoft HTML Object Library` का उपयोग करके HTML पार्स कर सकते हैं। अपने VBA संपादक में इस लाइब्रेरी के लिए एक संदर्भ जोड़ें, Tools > References पर जाकर और `Microsoft HTML Object Library` को चेक करें। इससे आपको HTML दस्तावेज़ों को नेविगेट और मैनिप्युलेट करने के लिए कक्षाओं तक पहुंच मिलती है।

यहाँ एक सरल उदाहरण है जो दिखाता है कि कैसे एक फाइल से HTML दस्तावेज़ लोड किया जाता है और सभी लिंक्स (एंकर टैग्स) को निकाला जाता है:

```vb
Sub ParseHTML()
    Dim htmlDoc As MSHTML.HTMLDocument
    Dim htmlElement As MSHTML.IHTMLElement
    Dim htmlElements As MSHTML.IHTMLElementCollection
    Dim htmlFile As String
    Dim fileContent As String
    
    ' फाइल से HTML सामग्री लोड करें
    htmlFile = "C:\path\to\your\file.html"
    Open htmlFile For Input As #1
    fileContent = Input$(LOF(1), 1)
    Close #1
    
    ' HTML दस्तावेज़ इनीशियलाइज़ करें
    Set htmlDoc = New MSHTML.HTMLDocument
    htmlDoc.body.innerHTML = fileContent
    
    ' सभी एंकर टैग्स प्राप्त करें
    Set htmlElements = htmlDoc.getElementsByTagName("a")

    ' सभी एंकर तत्वों के माध्यम से लूप करें और href विशेषता प्रिंट करें
    For Each htmlElement In htmlElements
        Debug.Print htmlElement.getAttribute("href")
    Next htmlElement
End Sub
```

यह स्क्रिप्ट एक HTML फाइल की सामग्री को पढ़ती है, इसे `HTMLDocument` ऑब्जेक्ट में लोड करती है, सभी एंकर तत्वों (`<a>` टैग्स) को प्राप्त करती है, और फिर उनके ऊपर पुनरावृत्ति करती है, प्रत्येक की `href` विशेषता को Immediate Window में प्रिंटिंग करती है।

## गहराई में:
ऐतिहासिक रूप से, VBA में HTML पार्स करना आधुनिक वेब स्क्रेपिंग और दस्तावेज़ संभालने की तकनीकों के लिए प्रत्यक्ष समर्थन की कमी के कारण थोड़ा कठिनाईपूर्ण रहा है। Microsoft HTML Object Library, भले ही शक्तिशाली हो, कुछ हद तक पुरानी है और नई तकनीकों की तरह आधुनिक वेब मानकों को चिकनाई से संभाल सकती है।

जटिल HTML पार्सिंग और वेब स्क्रेपिंग कार्यों के लिए, अक्सर Beautiful Soup या Scrapy जैसी लाइब्रेरी के साथ Python जैसे वैकल्पिक उपकरणों और भाषाओं की सिफारिश की जाती है। ये आधुनिक उपकरण अधिक लचीलेपन, बेहतर प्रदर्शन प्रदान करते हैं और वर्तमान वेब मानकों के साथ अधिक तालमेल रखते हैं। हालाँकि, Microsoft Office पारिस्थितिकी तंत्र के भीतर काम करते समय, Microsoft HTML Object Library के साथ VBA का उपयोग एक मूल्यवान कौशल बना रहता है। यह Excel और Access जैसे अनुप्रयोगों के साथ संगत रूप से HTML सामग्री का सीधा मैनिपुलेशन अनलॉक करता है, बुनियादी HTML दस्तावेज़ संभालने से संबंधित कार्यों को पूरा करने का एक सरल तरीका प्रदान करता है जिससे परिचित VBA वातावरण के बाहर कदम रखने की आवश्यकता नहीं होती।
