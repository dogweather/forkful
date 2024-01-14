---
title:                "Swift: Json के साथ काम करना"
simple_title:         "Json के साथ काम करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/working-with-json.md"
---

{{< edit_this_page >}}

## क्यों

जेसन (JSON) के साथ काम करने में किसी को रुचि रखने के लिए केवल 1-2 वाक्यों में क्यूँ काम करना चाहिए।

JSON (JavaScript Object Notation) एक प्रसिद्ध डेटा फॉर्मेट है जो कि web एप्लीकेशन्स में डेटा को आसानी से प्रसंस्करण करने के लिए उपयोग किया जाता है। यह स्थानीय फाइल के साथ काम करने के लिए कम्पैक्ट है और डेटा को network के बीच भेजने और प्राप्त करने में भी उपयोगी है।

## कैसे करें

```Swift
let studentJSON = """
{
  "name": "अमित",
  "age": 21,
  "major": "कम्प्यूटर साइंस",
  "courses": ["ऑपरेटिंग सिस्टम्स", "डेटाबेस", "वेब डेवलपमेंट"]
}
```

जेसन के साथ काम करना शुरू करने के लिए, हमें जेसन फाइल को लोड और पार्स करना होगा। हम `JSONDecoder` का उपयोग कर सकते हैं जो हमें जेसन डेटा को Swift objects में डिकोड करने में मदद करेगा।

```Swift
struct Student: Codable {
  let name: String
  let age: Int
  let major: String
  let courses: [String]
}

do {
  let decoder = JSONDecoder()
  let student = try decoder.decode(Student.self, from: studentJSON.data(using: .utf8)!)
  print(student.name)
  print(student.age)
  print(student.major)
  print(student.courses)
} catch {
  print(error)
}
```

उपरोक्त कोड के आउटपुट से हमें निम्न नतीजे मिलेंगे:

```Swift
अमित
21
कम्प्यूटर साइंस
["ऑपरेटिंग सिस्टम्स", "डेटाबेस", "वेब डेवलपमेंट"]
```

## गहराई में जाएं

हमने ऊपर दिखाए उदाहरण में जेसन डेटा को Swift object में पार्स किया है। अब जब भी हमें जेसन को डिकोड करने की जरूरत होगी, हमें सिर्फ `decode()` function का उपयोग करना होगा।

और गहराई में जाने के लिए, हम जेसन के अन्य व