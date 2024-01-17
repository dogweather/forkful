---
title:                "कंप्यूटर प्रोग्रामिंग पर एक लेख: एक पाठ फाइल लिखना।"
html_title:           "Swift: कंप्यूटर प्रोग्रामिंग पर एक लेख: एक पाठ फाइल लिखना।"
simple_title:         "कंप्यूटर प्रोग्रामिंग पर एक लेख: एक पाठ फाइल लिखना।"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

क्या यह क्या है और क्यों?:

टेक्स्ट फ़ाइल लिखने क्या है? यह एक साधन है जो कंप्यूटर प्रोग्रामर्स को अपने कोड और डेटा को स्टोर करने के लिए उपयोग में लाता है। यह एक आसान और सरल तरीका है अपनी डेटा को अपने सिस्टम में साफ़ रखने का।

कैसे:

```Swift
// एक फ़ाइल के लिए पथ बनाएं।
let filePath = "myFile.txt"
// नई फ़ाइल बनाएं या मौजूदा फ़ाइल को ओवरराइट करें।
let file = FileManager.default.createFile(atPath: filePath, contents: nil, attributes: nil)
// फ़ाइल में डेटा लिखें।
let dataToWrite = "मेरा पहला टेक्स्ट फ़ाइल।"
try dataToWrite.write(toFile: filePath, atomically: true, encoding: .utf8)
// फ़ाइल से डेटा पढ़ें।
let data = try String(contentsOfFile: filePath, encoding: .utf8)
print(data)
```

आप टेक्स्ट फ़ाइल में ग्लोबल जगह में डेटा रख सकते हैं और उसे दूसरे प्रोग्रामों या सिस्टम में भी उपयोग कर सकते हैं। आप फ़ाइल में जोड़ सकते हैं, एडिट कर सकते हैं और हटा सकते हैं।

घोर जाँच:

टेक्स्ट फ़ाइल का निर्माण पहले से ही बहुत पुराना है। आजकल, यह एक दलील है कि प्रोग्रामर सोच का एक स्पष्ट रूप है और कंप्यूटर के द्वारा उपयोग होने वाले विभिन्न प्रकार की हार्डवेयर पर बनाया गया है। यह एक जाँच है कि कोड क्या बनाया गया है, उसे सुधारें और उपयोगकर्ता से आसानी से साझा करें। अन्य विकल्प में संग्रह करने के लिए डेटा को स्थानांतरित करना होगा या एक मुख्य सिस्टम में स्थानांतरित करने के लिए कठिन तरीके पर जाना होगा।

इसके अलावा, आप टेक्स्ट फ़ाइल का उपयोग उन्नत तरीकों में भी कर सकते हैं, जैसे कि डेटा समारोह, जीसन जैसे वृद्धि और तकनीकी समारोह।

जुड़े प्रसंग:

- [Swift टुटोरियल: टेक्स्ट फ़ाइल कैसे लिखें और पढ़ें](https://www.tutorialspoint.com/swift/swift_text_file_handling.htm)
- [Swift डॉक्यूमेंटेशन: FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [आईओएस और macOS के फाइल और डायरेक्टरी प्रबंधन](https://developer.apple.com/library/archive/documentation/FileManagement/Conceptual/FileSystemProgrammingGuide/FileSystemOverview/FileSystemOverview.html)