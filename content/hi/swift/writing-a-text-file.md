---
title:                "एक पाठ फ़ाइल लिखना"
html_title:           "Swift: एक पाठ फ़ाइल लिखना"
simple_title:         "एक पाठ फ़ाइल लिखना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## क्यों
आपने सोचा है कि आप एक text file लिखना चाहते हैं और आपको ये समझ नहीं आ रहा है कि कैसे शुरू करें, तो आप बिल्कुल सही जगह पर हैं। एक text file लिखना Swift में बहुत आसान हो सकता है और हम यहां इसे कैसे करें इसके बारे में बात करेंगे।

## कैसे करें
चलिए समझते हैं कि text file लिखने के लिए हमें क्या करना होगा। सबसे पहले, हमें एक text file को create करना होगा और उसमें जो कुछ भी हमने लिखना है, उसे write करना होगा। लेकिन इससे पहले हमें `FileManager` इसका इस्तेमाल करके एक directory को create करना होगा। देखते हैं कैसे Swift में इसे implement किया जाता है:

```Swift
let fileManager = FileManager.default
let documents = fileManager.urls(for: .documentDirectory, in: .userDomainMask).first!
let fileName = "SampleFile"
let fileURL = documents.appendingPathComponent(fileName).appendingPathExtension("txt")

do {
    try fileManager.createDirectory(at: documents, withIntermediateDirectories: true, attributes: nil)
    try "This is a sample text file.".write(to: fileURL, atomically: true, encoding: .utf8)
    print("File created and written successfully!")
} catch {
    print("Error creating/writing file: \(error)")
}
```

ऊपर का कोड हमें एक text file `SampleFile.txt` को create करेगा और उसमें `This is a sample text file.` लिखेगा। इसके बाद हम `print` के इस्तेमाल से सफलतापूर्वक लिखने का संदेश प्राप्त कर सकते हैं।

## डीप डाइव
जब हम Swift में text file लिखते हैं तो ये `String` टाइप का डेटा हो सकता है और उसे आप किसी भी फॉर्मेट में encode कर सकते हैं। जैसे की हमने ऊपर `utf8` इस्तेमाल किया है, लेकिन आप `utf16` या `ascii` भी इस्तेमाल कर सकते हैं। इसके अलावा, आप अपने text file में लिखने के लिए कोई भी फॉर्मेट का इस्तेमाल कर सकते हैं और ये आपके requirement पर निर्भर है।

## देखें