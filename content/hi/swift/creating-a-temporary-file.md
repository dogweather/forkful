---
title:                "Swift: अस्थायी फ़ाइल बनाना"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# क्यों

अगर आप कोई स्विफ्ट प्रोग्रामर हैं तो आपने यह सुना होगा कि टेम्पररी फाइल कैसे बनाई जाती है। यह कुछ लोगों को परेशान करता है, लेकिन इसे बनाना अत्यंत उपयोगी हो सकता है। टेम्पररी फाइल बनाने के कुछ कारण हैं, जैसे कि किसी मूल्य को स्थानांतरित करने के लिए या किसी अन्य प्रोसेस को सहयोग करने के लिए। इस आर्टिकल में, हम टेम्पररी फाइल कैसे बनाते हैं इसके बारे में सीखेंगे।

# कैसे करें

यदि आपको कोई स्विफ्ट प्रोग्रामिंग का अनुभव है, तो आपने पता होगा कि टेम्पररी फाइल बनाने के लिए कई तरीके हैं। यहां हम आपको कुछ सरल और सुनिश्चित तरीके बताएंगे जिनसे आप आसानी से टेम्पररी फाइल बना सकते हैं, जैसे कि:

```Swift
// नए फाइल को बनाने के लिए पथ पानें
let tempDirectory = NSTemporaryDirectory()
let tempFileName = "NewFile.txt"
let tempFilePath = tempDirectory.appendingPathComponent(tempFileName)

// फाइल बनाएं
let fileManager = FileManager.default
fileManager.createFile(atPath: tempFilePath, contents: nil, attributes: nil)
print("नया टेम्पररी फाइल बनाई गई है।")
```

इस कोड के द्वारा, हम नए फाइल को बनाने के लिए पथ प्राप्त करते हैं और फिर उसे बनाते हैं। आप एक अलग तरीके से भी फाइल बना सकते हैं, लेकिन यहां हमने सबसे सरल तरीका दिखाया है।

# गहराई से जानें

टेम्पररी फाइल बनाना अधिकतर फाइल मैनेजमेंट या अन्य प्रोसेस को समर्थन करने के लिए किया जाता है। जब हम किसी प्रोसेस को