---
title:                "XML के साथ काम करना"
date:                  2024-01-26T04:35:37.641564-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML के साथ काम करना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/working-with-xml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
XML के साथ काम करना विस्तारित चिह्न भाषा में ढांचागत डेटा को संभालने और उस तक पहुँचने की प्रक्रिया है। प्रोग्रामर XML के साथ काम करते हैं ताकि अन्य प्रणालियों के साथ अंतरोपरियता सुनिश्चित की जा सके या कॉन्फिगरेशन फाइलों, डेटा फीड्स, और वेब सेवाओं में आम अन्य संरचित दस्तावेजों को पढ़ने और लिखने के लिए।

## कैसे करें:
```PowerShell
# एक XML फाइल को एक वेरिएबल में लोड करना
[xml]$xmlContent = Get-Content 'path\to\your\file.xml'

# XML नोड्स तक पहुँचना
$books = $xmlContent.catalog.book
foreach ($book in $books) {
  Write-Output "शीर्षक: $($book.title)"
}

# एक नया XML तत्व बनाना
$newBook = $xmlContent.CreateElement("book")
$newBook.SetAttribute("id", "bk999")
$xmlContent.DocumentElement.AppendChild($newBook)

# XML को वापस फाइल में सेव करना
$xmlContent.Save('path\to\your\updated\file.xml')
```
नमूना आउटपुट:
```
शीर्षक: प्रोग्रामिंग PowerShell
शीर्षक: XML आवश्यकताएँ
```

## गहराई से जानकारी
XML, या विस्तारित चिह्न भाषा, ९० के दशक के अंत से है और ढांचागत डेटा के लिए एक व्यापक रूप से इस्तेमाल किया जाने वाला प्रारूप बना हुआ है। PowerShell पारंपरिक पार्सिंग मेथड्स के मुकाबले XML के साथ काम करना सरल बनाता है; इसमें XML को सीधे ऑब्जेक्ट्स में कास्ट किया जाता है, जिससे आप सामान्य डॉट नोटेशन के माध्यम से तत्वों के साथ बातचीत कर सकते हैं।

XML के विकल्पों में JSON, YAML, या कस्टम डेटा प्रारूप शामिल हैं। उदाहरण के लिए, JSON अपनी हल्के प्रकृति और वेब तकनीकों के साथ इस्तेमाल की आसानी के लिए लोकप्रियता हासिल कर चुका है। हालांकि, XML की विस्तृत विशेषताएं जैसे कि नेमस्पेस, स्कीमास, और XSLT प्रोसेसिंग, इसे जटिल दस्तावेजों या उद्योग मानकों के लिए एक बेहतर विकल्प बनाते हैं।

PowerShell अपनी XML हैंडलिंग के लिए .NET फ्रेमवर्क की XML क्षमताओं का उपयोग करता है। इसका मतलब है कि यह केवल सामान्य पढ़ने-लिखने की क्रियाओं के बारे में नहीं है; आप PowerShell के माध्यम से वैलिडेशन के लिए XML स्कीमास के साथ काम कर सकते हैं, क्वेरीज के लिए XPath का उपयोग कर सकते हैं, और XSLT ट्रांसफॉर्मेशन्स को लागू कर सकते हैं।

## देखें भी
- [W3Schools XML ट्यूटोरियल](https://www.w3schools.com/xml/)
- [XML vs. JSON](https://www.json.org/json-en.html)