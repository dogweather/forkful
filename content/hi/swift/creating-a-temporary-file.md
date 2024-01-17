---
title:                "अस्थायी फ़ाइल बनाना"
html_title:           "Swift: अस्थायी फ़ाइल बनाना"
simple_title:         "अस्थायी फ़ाइल बनाना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों? 
कभी-कभी हम प्रोग्रामिंग में अस्थायी फाइलों को बनाने के साथ सामना करते हैं। ये अस्थायी फ़ाइलें तब बनाई जाती हैं जब हमें एक सामान्य फाइल की आवश्यकता होती है जो थोड़ी देर के लिए ही इस्तेमाल होगी। प्रोग्रामर्स अस्थायी फाइलें बनाते हैं क्योंकि इससे स्टोर की गई डेटा को साफ़ किया जाता है और ऐसे फ़ाइलों को ज़रूरत के बाद से हटा दिया जाता है। 

## कैसे: 
```Swift
let tempFile = FileManager.default.temporaryDirectory.appendingPathComponent("myTempFile.txt")
let tempFileData = "This is a temporary file!".data(using: .utf8)

do {
    try tempFileData?.write(to: tempFile)
    print("Temporary file successfully created at path: \(tempFile)")
} catch {
    print("Error creating temporary file: \(error)")
}
```

पहले, हम एक अस्थायी फाइल के लिए एक पथ का उपयोग करके `FileManager` का उपयोग करते हैं। फिर हम फ़ाइल में डेटा लिख कर इसे अस्थायी बनाते हैं। अंत में, हमें फ़ाइल बनाने की प्रक्रिया में कोई त्रुटि आए तो संबंधित त्रुटि को प्रिंट करना होगा। 

## गहराई में जाएं: 
अस्थायी फ़ाइलों को बनाने की इतिहासिक पृष्टभूमि उनके मूल उद्देश्य के साथ जुड़ी है। इसके अलावा, दूसरे विकल्प शामिल हैं जो अस्थायी फ़ाइलों के स्थान पर उपयोग किए जा सकते हैं, जैसे आधारभूत फ़ाइल सिस्टम। अस्थायी फ़ाइलें सामान्यतया इंटरनेट कनेक्शन से ज़्यादा तेज़ होती हैं और अस्थायी फ़ाइलों को पुनर्प्रयोग किया जा सकता है, जो फाइल सिस्टम से बचाने के लिए अच्छा होता है।

## इसके अलावा: 
संबंधित स्रोतों में अधिक जानकारी प्राप्त करने के लिए नीचे दिए गए लिंकों का उपयोग करें: 
- [Apple दस्तावेज़](https://developer.apple.com/documentation/foundation/1561527-createtemporaryfile)
- [NSHipster](https://nshipster.com/temporary-files/)