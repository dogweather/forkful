---
title:    "Swift: कंप्यूटर प्रोग्रामिंग पर एक लेख: एक टेक्स्ट फ़ाइल लिखना"
keywords: ["Swift"]
---

{{< edit_this_page >}}

अभी तक हमने स्विफ्ट प्रोग्रामिंग से सभी अधिकतम तरीकों का चर्चा किया है, लेकिन क्या आपने कभी सोचा है कि आप टेक्स्ट फ़ाइल्स के साथ खेल सकते हैं? इस आर्टिकल में, हम इस बात पर ध्यान देंगे कि टेक्स्ट फ़ाइल क्यों और कैसे लिखा जाता है।

## क्यों

कोई भी प्रोग्रामर, अपने कोड को सुलभ स्थान पर संग्रहित रखना चाहेंगा ताकि उन्हें उसे खोजने और उसे अद्यतन करने में आसानी हो। टेक्स्ट फ़ाइलें इस मामले में बेहतर विकल्प हो सकती हैं, क्योंकि वे हमेशा उपलब्ध होती हैं और किसी भी प्रोग्रामिंग भाषा में लिखी जा सकती हैं। इसलिए, टेक्स्ट फ़ाइलों को सतह पर उपलब्ध करना संभव हो सकता है।

## कैसे करें

अब आप स्विफ्ट में टेक्स्ट फ़ाइल लिखने का तरीका सीखने के लिए तैयार हैं! पहले, हमें एक नया टेक्स्ट फ़ाइल बनानी होगी। इसके लिए हम "FileManager.default.createFile" फ़ंक्शन का उपयोग कर सकते हैं। इसका उपयोग करके हम एक नया फ़ाइल बना सकते हैं और उसे डिस्क पर लिख सकते हैं।

```Swift
let fileManager = FileManager.default
let fileURL = fileManager.urls(for: .desktopDirectory, in: .userDomainMask)[0].appendingPathComponent("myFile.txt")
fileManager.createFile(atPath: fileURL.path, contents: nil, attributes: nil)
```

यहां, हमने "myFile.txt" नाम की एक टेक्स्ट फ़ाइल बनाई है और उसे अपने डेस्कटॉप पर सहेजा है। आप अपनी फ़ाइल को किसी भी अन्य नाम और स्थान पर सहेज सकते हैं।