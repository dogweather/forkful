---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:28.589724-07:00
description: "\u0915\u0948\u0938\u0947: PowerShell \u092E\u0947\u0902 \u092E\u0942\
  \u0932 \u0930\u0942\u092A \u0938\u0947 \u0915\u094B\u0908 \u0938\u092E\u0930\u094D\
  \u092A\u093F\u0924 HTML \u092A\u093E\u0930\u094D\u0938\u0930 \u0928\u0939\u0940\u0902\
  \ \u0939\u0948, \u0932\u0947\u0915\u093F\u0928 \u0906\u092A `Invoke-WebRequest`\
  \ cmdlet \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947 HTML\
  \ \u0938\u093E\u092E\u0917\u094D\u0930\u0940 \u0924\u0915 \u092A\u0939\u0941\u0902\
  \u091A \u0938\u0915\u0924\u0947 \u0939\u0948\u0902 \u0914\u0930 \u0909\u0938\u0947\
  \u2026"
lastmod: '2024-03-13T22:44:52.698599-06:00'
model: gpt-4-0125-preview
summary: "PowerShell \u092E\u0947\u0902 \u092E\u0942\u0932 \u0930\u0942\u092A \u0938\
  \u0947 \u0915\u094B\u0908 \u0938\u092E\u0930\u094D\u092A\u093F\u0924 HTML \u092A\
  \u093E\u0930\u094D\u0938\u0930 \u0928\u0939\u0940\u0902 \u0939\u0948, \u0932\u0947\
  \u0915\u093F\u0928 \u0906\u092A `Invoke-WebRequest` cmdlet \u0915\u093E \u0909\u092A\
  \u092F\u094B\u0917 \u0915\u0930\u0915\u0947 HTML \u0938\u093E\u092E\u0917\u094D\u0930\
  \u0940 \u0924\u0915 \u092A\u0939\u0941\u0902\u091A \u0938\u0915\u0924\u0947 \u0939\
  \u0948\u0902 \u0914\u0930 \u0909\u0938\u0947 \u092A\u093E\u0930\u094D\u0938 \u0915\
  \u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964 \u0905\u0927\u093F\u0915\
  \ \u091C\u091F\u093F\u0932 \u092A\u093E\u0930\u094D\u0938\u093F\u0902\u0917 \u0914\
  \u0930 \u092E\u0948\u0928\u093F\u092A\u0941\u0932\u0947\u0936\u0928 \u0915\u0947\
  \ \u0932\u093F\u090F, HtmlAgilityPack, \u090F\u0915 \u0932\u094B\u0915\u092A\u094D\
  \u0930\u093F\u092F .NET \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940,\
  \ \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u093F\u092F\u093E \u091C\u093E\
  \ \u0938\u0915\u0924\u093E \u0939\u0948\u0964\n\n#."
title: "HTML \u0935\u093F\u0936\u094D\u0932\u0947\u0937\u0923"
weight: 43
---

## कैसे:
PowerShell में मूल रूप से कोई समर्पित HTML पार्सर नहीं है, लेकिन आप `Invoke-WebRequest` cmdlet का उपयोग करके HTML सामग्री तक पहुंच सकते हैं और उसे पार्स कर सकते हैं। अधिक जटिल पार्सिंग और मैनिपुलेशन के लिए, HtmlAgilityPack, एक लोकप्रिय .NET लाइब्रेरी, का उपयोग किया जा सकता है।

### `Invoke-WebRequest` का उपयोग करना:
```powershell
# एक वेबपेज से शीर्षक फेच करने के लिए सरल उदाहरण
$response = Invoke-WebRequest -Uri 'http://example.com'
# DOM तत्वों तक पहुंचने के लिए ParsedHtml प्रॉपर्टी का इस्तेमाल करें
$title = $response.ParsedHtml.title
Write-Output $title
```

नमूना आउटपुट:

```
उदाहरण डोमेन
```

### HtmlAgilityPack का उपयोग करना:
सबसे पहले, आपको HtmlAgilityPack इंस्टॉल करना होगा। आप इसे NuGet पैकेज मैनेजर के माध्यम से कर सकते हैं:

```powershell
Install-Package HtmlAgilityPack -ProviderName NuGet
```

फिर, आप इसका उपयोग PowerShell में HTML पार्स करने के लिए कर सकते हैं:

```powershell
# HtmlAgilityPack असेंबली लोड करें
Add-Type -Path "path\to\HtmlAgilityPack.dll"

# एक HtmlDocument ऑब्जेक्ट बनाएँ
$doc = New-Object HtmlAgilityPack.HtmlDocument

# एक फाइल या वेब रिक्वेस्ट से HTML लोड करें
$htmlContent = (Invoke-WebRequest -Uri "http://example.com").Content
$doc.LoadHtml($htmlContent)

# तत्वों को निकालने के लिए XPath या अन्य क्वेरी मेथड्स का उपयोग करें
$node = $doc.DocumentNode.SelectSingleNode("//h1")

if ($node -ne $null) {
    Write-Output $node.InnerText
}
```

नमूना आउटपुट:

```
वेलकम टू Example.com!
```

इन उदाहरणों में, `Invoke-WebRequest` सरल कार्यों के लिए बेहतर है, जबकि HtmlAgilityPack जटिल HTML पार्सिंग और मैनिपुलेशन के लिए कहीं अधिक समृद्ध सुविधाओं का प्रस्ताव करता है।
