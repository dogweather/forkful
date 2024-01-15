---
title:                "Json के साथ काम करना"
html_title:           "Swift: Json के साथ काम करना"
simple_title:         "Json के साथ काम करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/working-with-json.md"
---

{{< edit_this_page >}}

## क्यों

क्या आप Swift में काम करना चाहते हैं और आपको JSON के साथ काम करने के बारे में जानना चाहते हैं? यह आर्टिकल आपको क्यों JSON को समझना है और उसका उपयोग करना है आपके लिए जानकारी देगा।

## कैसे करें

कभी-कभी हमारे Swift एप्लिकेशन को विभिन्न प्लेटफॉर्मों से डेटा जुड़ाने के लिए हमें JSON का उपयोग करना पड़ता है। स्विफ्ट 4 के साथ, हमें प्रोटोकॉल `Codable` के माध्यम से डेटा को JSON में सीरियलाइज और डिसीरियलाइज करने के लिए निर्देशित किया जाता है। यह हमें कई लाइनों के कोड के बजाय अधिक स्पष्ट तरीके से डेटा की प्रतिनिधि करता है। आइए यह जानते हैं कि हमें JSON को कैसे समझना है और इसका उपयोग करना है।

इसके लिए, हमें पहले एक साधारण `struct` बनानी होगी और उसमें आवश्यक प्रॉपर्टी जोड़नी होगी, जोहुमें हम JSON से डेटा प्राप्त करना होगा।

```Swift
struct Book: Codable {
    let title: String
    let author: String
}
```

अब हमें एक नया बुक बनाना होगा और उसे `JSONEncoder` का उपयोग करके एक JSON आइटम में सीरियलाइज करना होगा।

```Swift
let book = Book(title: "The Alchemist", author: "Paulo Coelho")
let jsonEncoder = JSONEncoder()
let jsonData = try! jsonEncoder.encode(book)
```

यह हमें एक `Data` आइटम देगा, जिसे हम `print` करके देख सकते हैं।

```
print(jsonData)

// Output: 38 bytes
```

अब हमें अपने JSON डेटा को `JSONDecoder` का उपयोग करके डिसीरियलाइज करना होगा। इसके लिए, हमें सिर्फ `Codable` टाइप के माध्यम से नया `struct` बनाना हो