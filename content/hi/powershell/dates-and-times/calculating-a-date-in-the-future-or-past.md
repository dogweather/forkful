---
date: 2024-01-20 17:32:42.407392-07:00
description: "\u092D\u0935\u093F\u0937\u094D\u092F \u092F\u093E \u0905\u0924\u0940\
  \u0924 \u0915\u0940 \u0924\u093E\u0930\u0940\u0916 \u0915\u0940 \u0917\u0923\u0928\
  \u093E \u0915\u0930\u0928\u093E \u0938\u093E\u0927\u093E\u0930\u0923 \u0935\u093F\
  \u091A\u093E\u0930 \u0939\u0948 - \u0939\u092E \u0935\u0930\u094D\u0924\u092E\u093E\
  \u0928 \u0924\u093E\u0930\u0940\u0916 \u0938\u0947 \u0915\u0941\u091B \u0926\u093F\
  \u0928, \u0938\u092A\u094D\u0924\u093E\u0939, \u092E\u093E\u0939, \u092F\u093E \u0935\
  \u0930\u094D\u0937 \u091C\u094B\u0921\u093C\u0924\u0947 \u092F\u093E \u0918\u091F\
  \u093E\u0924\u0947 \u0939\u0948\u0902\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\
  \u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0947 \u0928\u093F\u092F\u094B\
  \u091C\u0928, \u0938\u092E\u092F\u2026"
lastmod: '2024-03-13T22:44:52.725709-06:00'
model: gpt-4-1106-preview
summary: "\u092D\u0935\u093F\u0937\u094D\u092F \u092F\u093E \u0905\u0924\u0940\u0924\
  \ \u0915\u0940 \u0924\u093E\u0930\u0940\u0916 \u0915\u0940 \u0917\u0923\u0928\u093E\
  \ \u0915\u0930\u0928\u093E \u0938\u093E\u0927\u093E\u0930\u0923 \u0935\u093F\u091A\
  \u093E\u0930 \u0939\u0948 - \u0939\u092E \u0935\u0930\u094D\u0924\u092E\u093E\u0928\
  \ \u0924\u093E\u0930\u0940\u0916 \u0938\u0947 \u0915\u0941\u091B \u0926\u093F\u0928\
  , \u0938\u092A\u094D\u0924\u093E\u0939, \u092E\u093E\u0939, \u092F\u093E \u0935\u0930\
  \u094D\u0937 \u091C\u094B\u0921\u093C\u0924\u0947 \u092F\u093E \u0918\u091F\u093E\
  \u0924\u0947 \u0939\u0948\u0902\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\
  \u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0947 \u0928\u093F\u092F\u094B\u091C\
  \u0928, \u0938\u092E\u092F\u2026"
title: "\u092D\u0935\u093F\u0937\u094D\u092F \u092F\u093E \u0905\u0924\u0940\u0924\
  \ \u092E\u0947\u0902 \u0924\u093E\u0930\u0940\u0916 \u0915\u0940 \u0917\u0923\u0928\
  \u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?

भविष्य या अतीत की तारीख की गणना करना साधारण विचार है - हम वर्तमान तारीख से कुछ दिन, सप्ताह, माह, या वर्ष जोड़ते या घटाते हैं। प्रोग्रामर्स इसे नियोजन, समय प्रबंधन और पुराने डाटा के विश्लेषण के लिए करते हैं।

## कैसे:

### भविष्य की तारीख की गणना:
```PowerShell
# 10 दिन बाद की तारीख
$futureDate = (Get-Date).AddDays(10)
Write-Output $futureDate
```

### अतीत की तारीख:
```PowerShell
# 10 दिन पहले की तारीख
$pastDate = (Get-Date).AddDays(-10)
Write-Output $pastDate
```

### सैंपल आउटपुट:
```PowerShell
सोमवार, 17 अप्रैल, 2023 14:32:21
```

## गहराई से जानकारी:

डेट की गणना न केवल योजनाओं के लिए महत्वपूर्ण है, बल्कि अनुप्रयोग के लिए भी ज़रूरी होती है, जैसे की डाटाबेस का समय-आधारित प्रेरण (triggering) और कैलेंडर अनुप्रयोग। ऐतिहासिक रूप से, तारीखों के मैनुअल परिवर्तन से बहुत समय लगता था और गलतियां होने की संभावना भी रहती थी। PowerShell और अन्य प्रोग्रामिंग भाषाओं के आने से यह कार्य बहुत आसान और सटीक हो गया है।

लिनक्स शेल जैसे विकल्प तारीख की गणना के लिए `date` कमांड प्रदान करते हैं। जबकि SQL में `DATEADD()` और `DATEDIFF()` जैसे फंक्शंस हैं। लेकिन PowerShell में `Get-Date` का `AddDays()`, `AddHours()`, `AddMinutes()` आदि मेथोड का प्रयोग करने से हम बहुत ही सहजता से तारीखों की गणना कर सकते हैं। 

इसके विस्तृत उपयोगिता में, हम लीप इयर, टाइम ज़ोन्स की जटिलताओं, और डेट-टाइम फॉरमेटिंग के विभिन्न मानकों को भी संभाल सकते हैं, जिससे कि अनुप्रयोग विश्वव्यापी स्तर पर सही काम कर सके।

## यह भी देखें:

- PowerShell कार्यशील उदाहरण: [PowerShell Gallery](https://www.powershellgallery.com/)
- अंतर्राष्ट्रीय मानकों के बारे में: [ISO 8601 Date and time format](https://www.iso.org/iso-8601-date-and-time-format.html)
