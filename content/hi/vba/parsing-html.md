---
title:                "HTML पार्स करना"
date:                  2024-02-01T21:59:01.767099-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML पार्स करना"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/vba/parsing-html.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

Visual Basic for Applications (VBA) में HTML पार्स करना एक HTML दस्तावेज़ से विशिष्ट जानकारी निकालने की प्रक्रिया है। प्रोग्रामर इसे वेब पृष्ठों से डेटा पढ़ने और संभालने की प्रक्रिया को स्वचालित करने के लिए करते हैं, जैसे कि वेबसाइट कंटेंट स्क्रेप करना या फॉर्म सबमिशन और डेटा प्राप्ति को स्वतः करना, VBA का समर्थन करने वाले अनुप्रयोगों के भीतर जैसे कि Microsoft Excel या Access में।

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
