---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:00.251481-07:00
description: "CSV (Comma-Separated Values) \u092B\u093C\u093E\u0907\u0932\u094B\u0902\
  \ \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E \u090F\
  \u0915 \u0906\u092E \u0915\u093E\u0930\u094D\u092F \u0939\u0948 \u091C\u094B \u0921\
  \u0947\u091F\u093E \u0915\u094B \u0938\u0902\u0930\u091A\u093F\u0924, \u0924\u093E\
  \u0932\u093F\u0915\u093E \u0930\u0942\u092A \u092E\u0947\u0902 \u092A\u094D\u0930\
  \u092C\u0902\u0927\u093F\u0924 \u0914\u0930 \u0938\u0902\u0936\u094B\u0927\u093F\
  \u0924 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0915\u093F\u092F\
  \u093E \u091C\u093E\u0924\u093E \u0939\u0948\u0964\u2026"
lastmod: '2024-03-13T22:44:52.740865-06:00'
model: gpt-4-0125-preview
summary: "CSV (Comma-Separated Values) \u092B\u093C\u093E\u0907\u0932\u094B\u0902\
  \ \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E \u090F\
  \u0915 \u0906\u092E \u0915\u093E\u0930\u094D\u092F \u0939\u0948 \u091C\u094B \u0921\
  \u0947\u091F\u093E \u0915\u094B \u0938\u0902\u0930\u091A\u093F\u0924, \u0924\u093E\
  \u0932\u093F\u0915\u093E \u0930\u0942\u092A \u092E\u0947\u0902 \u092A\u094D\u0930\
  \u092C\u0902\u0927\u093F\u0924 \u0914\u0930 \u0938\u0902\u0936\u094B\u0927\u093F\
  \u0924 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0915\u093F\u092F\
  \u093E \u091C\u093E\u0924\u093E \u0939\u0948\u0964\u2026"
title: "CSV \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?

CSV (Comma-Separated Values) फ़ाइलों के साथ काम करना एक आम कार्य है जो डेटा को संरचित, तालिका रूप में प्रबंधित और संशोधित करने के लिए किया जाता है। प्रोग्रामर अक्सर इस ऑपरेशन को विभिन्न एप्लीकेशनों, जैसे कि डेटा विश्लेषण, रिपोर्टिंग या यहां तक कि वेब एप्लिकेशनों को चालू करने के लिए डेटा को आयात, निर्यात, या संशोधित करने के लिए करते हैं।

## कैसे:

### एक CSV फ़ाइल को पढ़ना

एक CSV फाइल से पढ़ने के लिए, `Import-Csv` कमांडलेट का उपयोग करें। यह कमांडलेट फाइल को पढ़ता है और प्रत्येक पंक्ति के लिए कस्टम PowerShell ऑब्जेक्ट्स में परिवर्तित करता है।

```powershell
# एक CSV फ़ाइल को आयात करना
$data = Import-Csv -Path "C:\Data\users.csv"
# सामग्री प्रदर्शित करना
$data
```

**नमूना आउटपुट:**

```
नाम    आयु    शहर
----    ---    ----
जॉन    23     न्यू यॉर्क
डो     29     लॉस एंजेलिस
```

### एक CSV फाइल में लिखना

इसके विपरीत, एक CSV फाइल में डेटा लिखने के लिए, `Export-Csv` कमांडलेट का उपयोग किया जाता है। यह कमांडलेट इनपुट ऑब्जेक्ट्स को लेता है और उन्हें CSV स्वरूप में बदल देता है।

```powershell
# निर्यात के लिए एक ऑब्जेक्ट बनाना
$users = @(
    [PSCustomObject]@{Name='जॉन'; Age='23'; City='न्यू यॉर्क'},
    [PSCustomObject]@{Name='डो'; Age='29'; City='लॉस एंजेलिस'}
)

# एक CSV फ़ाइल में निर्यात करना
$users | Export-Csv -Path "C:\Data\new_users.csv" -NoTypeInformation
```

निष्पादित होने के बाद, `new_users.csv` नाम की एक फ़ाइल बनाई गई है जिसमें प्रदान किया गया डेटा है।

### CSV सामग्री को फ़िल्टर करना और संशोधित करना

CSV फ़ाइल से डेटा फ़िल्टर करने या संशोधित करने के लिए, PowerShell की ऑब्जेक्ट मैनिपुलेशन क्षमताओं का उपयोग करें। उदाहरण के लिए, केवल उन उपयोगकर्ताओं को चुनने के लिए जिनकी आयु एक निश्चित सीमा से ऊपर है और जो एक विशेष शहर से हैं:

```powershell
# डेटा आयात करना और फ़िल्टर करना
$filteredData = Import-Csv -Path "C:\Data\users.csv" | Where-Object {
    $_.Age -gt 25 -and $_.City -eq 'लॉस एंजेलिस'
}

# फ़िल्टर किया गया डेटा प्रदर्शित करना
$filteredData
```

**नमूना आउटपुट:**

```
नाम    आयु    शहर
----    ---    ----
डो     29     लॉस एंजेलिस
```

### तृतीय-पक्ष पुस्तकालयों का उपयोग

जबकि PowerShell के मूल कमांडलेट अक्सर आम कार्यों के लिए पर्याप्त होते हैं, अधिक जटिल ऑपरेशंस तृतीय-पक्ष पुस्तकालयों या उपकरणों से लाभान्वित हो सकते हैं। हालांकि, मानक CSV संशोधन के लिए, जैसे कि पढ़ना, लिखना, फ़िल्टर करना, या सॉर्टिंग, PowerShell के बिल्ट-इन कमांडलेट जैसे `Import-Csv` और `Export-Csv` आम तौर पर अतिरिक्त पुस्तकालयों की आवश्यकता के बिना दृढ़ कार्यक्षमता प्रदान करते हैं।
