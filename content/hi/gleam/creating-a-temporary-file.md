---
title:                "अस्थायी फाइल बनाना"
date:                  2024-01-20T17:40:50.378503-07:00
model:                 gpt-4-1106-preview
simple_title:         "अस्थायी फाइल बनाना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
अस्थाई फाइल बनाना एक ऐसी प्रक्रिया है जिसमें हम डेटा को अस्थायी रूप से संग्रहित करते हैं। प्रोग्रामर्स इसे डेटा प्रोसेसिंग, टेस्टिंग, या कैशिंग जैसे कार्यों के लिए करते हैं।

## How to: (कैसे करें:)
```gleam
// Gleam में अस्थाई फाइल बनाने का कोड अब तक उपलब्ध नहीं है
// यहां मैं एक हाइपोथेटिकल उदाहरण दिखा रहा हूं
// कृपया नए जारी किए गए डॉक्यूमेंटेशन की जांच करें

import gleam/io
import gleam/expect
import my/cool_lib

pub fn create_temp_file(contents: String) -> Result {
  // यहाँ आपका लॉजिक होगा
  let temp_file = cool_lib.make_temp_file(contents)
  // अस्थाई फाइल बनाएं और परिणाम वापस करें
  expect.ok(temp_file)
}

// हाइपोथेटिकल उपयोग
pub fn demo_temp_file_use() {
  let result = create_temp_file("यहाँ आपका डेटा है".to_string())
  io.println(result)
}
```

सैंपल आउटपुट:
```
Ok(<temp_file_path>)
```

## Deep Dive (गहन जानकारी)
अस्थाई फाइलें एक पुरानी सोच हैं, UNIX लाइक सिस्टम्स में `/tmp` डायरेक्टरी हमें इसकी याद दिलाती है। ग्लीम में, अस्थाई फाइलें बनाने के लिए stdlib हो सकता है, लेकिन फिलहाल यह स्टैंडर्ड फीचर नहीं है। एल्टरनेटिव्स में सिस्टम कॉल्स या बाहरी पैकेजेज का उपयोग शामिल होगा। एक सुरक्षित और स्थिर इंप्लीमेंटेशन के लिए, अस्थाई फाइलों को अद्वितीय नाम दिया जाता है और अक्सर स्वचालित रूप से मिटाया जाता है।

## See Also (और भी देखें)
- बाहरी पैकेज रेपॉजिटरी: [https://hex.pm/](https://hex.pm/)
- यूनिक्स `/tmp` डायरेक्टरी और इसका उपयोग: [https://en.wikipedia.org/wiki//tmp](https://en.wikipedia.org/wiki//tmp)