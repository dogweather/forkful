---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:13:40.184308-07:00
description: "\u090F\u0938\u094B\u0938\u093F\u090F\u091F\u093F\u0935 \u0910\u0930\u0947\
  , \u091C\u093F\u0928\u094D\u0939\u0947\u0902 PowerShell \u092E\u0947\u0902 \u0939\
  \u0948\u0936 \u091F\u0947\u092C\u0932\u094D\u0938 \u092F\u093E \u0921\u093F\u0915\
  \u094D\u0936\u0928\u0930\u0940\u091C\u093C \u092D\u0940 \u0915\u0939\u093E \u091C\
  \u093E\u0924\u093E \u0939\u0948, \u0906\u092A\u0915\u094B \u0921\u093E\u091F\u093E\
  \ \u0915\u094B \u0915\u0940-\u0935\u0948\u0932\u094D\u092F\u0942 \u091C\u094B\u0921\
  \u093C\u094B\u0902 \u092E\u0947\u0902 \u0938\u0902\u0917\u094D\u0930\u0939\u093F\
  \u0924 \u0915\u0930\u0928\u0947 \u0926\u0947\u0924\u0947 \u0939\u0948\u0902, \u091C\
  \u093F\u0938\u0938\u0947 \u0921\u093E\u091F\u093E \u0915\u0940\u2026"
lastmod: '2024-03-13T22:44:52.686840-06:00'
model: gpt-4-0125-preview
summary: "\u090F\u0938\u094B\u0938\u093F\u090F\u091F\u093F\u0935 \u0910\u0930\u0947\
  , \u091C\u093F\u0928\u094D\u0939\u0947\u0902 PowerShell \u092E\u0947\u0902 \u0939\
  \u0948\u0936 \u091F\u0947\u092C\u0932\u094D\u0938 \u092F\u093E \u0921\u093F\u0915\
  \u094D\u0936\u0928\u0930\u0940\u091C\u093C \u092D\u0940 \u0915\u0939\u093E \u091C\
  \u093E\u0924\u093E \u0939\u0948, \u0906\u092A\u0915\u094B \u0921\u093E\u091F\u093E\
  \ \u0915\u094B \u0915\u0940-\u0935\u0948\u0932\u094D\u092F\u0942 \u091C\u094B\u0921\
  \u093C\u094B\u0902 \u092E\u0947\u0902 \u0938\u0902\u0917\u094D\u0930\u0939\u093F\
  \u0924 \u0915\u0930\u0928\u0947 \u0926\u0947\u0924\u0947 \u0939\u0948\u0902, \u091C\
  \u093F\u0938\u0938\u0947 \u0921\u093E\u091F\u093E \u0915\u0940\u2026"
title: "\u0938\u0939\u092F\u094B\u0917\u0940 \u0905\u0930\u0947\u091C\u093C \u0915\
  \u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?

एसोसिएटिव ऐरे, जिन्हें PowerShell में हैश टेबल्स या डिक्शनरीज़ भी कहा जाता है, आपको डाटा को की-वैल्यू जोड़ों में संग्रहित करने देते हैं, जिससे डाटा की प्राप्ति सरल और कुशल बनती है। प्रोग्रामर इसे इस तरह संबंधित डेटा को साथ में संग्रहित करने के लिए उपयोग करते हैं जो कि की के द्वारा पहुँच में आसान हो।

## कैसे करें:

PowerShell में एसोसिएटिव ऐरे का निर्माण और उपयोग काफी सीधा है। यहाँ है कैसे आप जादू करते हैं:

**एसोसिएटिव ऐरे बनाना:**

```PowerShell
$myAssociativeArray = @{}
$myAssociativeArray["name"] = "Alex"
$myAssociativeArray["age"] = 25
$myAssociativeArray["job"] = "Engineer"
```

यह कोड स्निपेट तीन की-वैल्यू जोड़ों के साथ एक एसोसिएटिव ऐरे बनाता है।

**मानों तक पहुँचना:**

एक मान प्राप्त करने के लिए, उसकी की का संदर्भ लें:

```PowerShell
Write-Output $myAssociativeArray["name"]
```

**नमूना आउटपुट:**

```
Alex
```

**डेटा जोड़ना या संशोधन करना:**

केवल एक नई जोड़ी जोड़ने या मौजूदा एक को संशोधित करने के लिए की का उपयोग करें:

```PowerShell
$myAssociativeArray["location"] = "New York" # एक नई की-वैल्यू जोड़ी जोड़ता है
$myAssociativeArray["job"] = "Senior Engineer" # एक मौजूदा जोड़ी को संशोधित करता है
```

**एक एसोसिएटिव ऐरे पर इटरेटिंग:**

ऐसे कीज़ और मानों पर लूप लगाएं:

```PowerShell
foreach ($key in $myAssociativeArray.Keys) {
  $value = $myAssociativeArray[$key]
  Write-Output "$key : $value"
}
```

**नमूना आउटपुट:**

```
name : Alex
age : 25
job : Senior Engineer
location : New York
```

## गहराई में जाना

एसोसिएटिव ऐरे की अवधारणा कई प्रोग्रामिंग भाषाओं में सामान्य होती है, आमतौर पर इसे भाषा के आधार पर डिक्शनरी, मैप, या हैश टेबल कहा जाता है। PowerShell में, एसोसिएटिव ऐरे हैश टेबल्स के रूप में लागू किए जाते हैं, जो कि कीज़ की खोज, डेटा भंडारण, और अनूठी कीज़ के संग्रह को बनाए रखने के लिए काफी कुशल होते हैं।

ऐतिहासिक रूप से, एसोसिएटिव ऐरे ऑब्जेक्ट्स के संग्रहों को प्रबंधित करने का एक साधन प्रदान करते हैं जहाँ प्रत्येक आइटम को अपनी की का उपयोग करके संपूर्ण संग्रह के माध्यम से इटरेटिंग किए बिना जल्दी से प्राप्त किया जा सकता है। एसोसिएटिव ऐरे में डेटा प्राप्ति और संशोधन की कुशलता उन्हें विविध कार्यों के लिए पसंदीदा विकल्प बनाती है। हालांकि, उनकी कुछ सीमाएँ होती हैं, जैसे कि क्रम बनाए रखना, जिनके लिए ऑर्डर्ड डिक्शनरीज़ या कस्टम ऑब्जेक्ट्स एक बेहतर विकल्प हो सकते हैं।

उनकी सीमाओं के बावजूद, PowerShell में एसोसिएटिव ऐरे/हैश टेबल्स अत्यंत लचीले होते हैं और स्क्रिप्टिंग के लिए एक शक्तिशाली उपकरण होते हैं। वे गतिशील डेटा संग्रहण की अनुमति देते हैं और विशेष रूप से विन्यासों, डेटा मेनिपुलेशन में, और कहीं भी संरचित डेटा प्रारूप की आवश्यकता होती है बिना किसी औपचारिक क्लास परिभाषा के बोझ के बिना उपयोगी होते हैं। बस याद रखें, एसोसिएटिव ऐरे की-आधारित प्राप्ति के लिए उत्तम होते हैं, यदि आपका कार्य जटिल डेटा संरचनाओं की मांग करता है या एक विशिष्ट क्रम को बनाए रखना चाहता है, तो आपको PowerShell के अन्य डेटा प्रकारों या कस्टम ऑब्जेक्ट्स की खोज करनी चाहिए।
