---
title:                "एक टेक्स्ट फाइल को पढ़ना"
html_title:           "TypeScript: एक टेक्स्ट फाइल को पढ़ना"
simple_title:         "एक टेक्स्ट फाइल को पढ़ना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## क्यों

टेक्स्ट फ़ाइल पढ़ना एक बहुत ही स्पष्ट और उपयोगी कार्य है। इससे हम एकाधिक डेटा और जानकारी को संग्रहीत कर सकते हैं जो हमारे कॉड को और समजने में मदद करता है। यह एक प्रैक्टिसल स्किल है जो हर प्रोग्रामर को सीखनी चाहिए।

## कैसे करें

टेक्स्ट फ़ाइल पढ़ने के लिए, हम `fs` नेजमोड्यूल का उपयोग कर सकते हैं। इसमें `createReadStream` और `readFile` फ़ंक्शन शामिल हैं जो हमें टेक्स्ट फ़ाइल में उपलब्ध डेटा को पढ़ने में मदद करते हैं। नीचे एक साधारण उदाहरण दिया गया है:

```TypeScript
import fs from "fs";

// Sync read
const data = fs.readFileSync("sample.txt", "utf-8");
console.log(data);

// Async read
fs.readFile("sample.txt", "utf-8", (err, data) => {
  if (err) throw err;
  console.log(data);
});
```

आउटपुट:

```
This is some sample text!
```

## डीप डाइव

टेक्स्ट फ़ाइल पढ़ने का कोड अक्सर संभवतः अन्य ऑपरेशंस के साथ अपने डेटा को प्रोसेस करने के लिए उपयोगी होता है। हम इसे स्ट्रिंग, JSON, या अन्य डेटा संरचनाओं में पार्स कर सकते हैं और अपने बिजनेस लॉजिक को लागू कर सकते हैं। इसलिए, टेक्स्ट फ़ाइल पढ़ना हमारे लिए बहुत ही अहम है।

## देखें

- [Node.js डॉक्यूमेंटेशन](https://nodejs.org/api/fs.html#fs_fs_readfile_path_options_callback)
- [यूपीएमई के टाइप स्क्रिप्ट गाइड](https://github.com/Microsoft/TypeScript-New-Handbook/blob/master/doc/README-hi.md)