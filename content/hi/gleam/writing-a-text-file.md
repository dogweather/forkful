---
title:    "Gleam: कंप्यूटर प्रोग्रामिंग में एक टेक्स्ट फाइल लिखने का तरीका"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## क्यों
एक पाठ फ़ाइल लिखने का कार्य  क्यों किया जाता है?

पाठ फ़ाइल लिखना एक अहम उपकरण है जो किसी भी प्रोग्रामिंग भाषा के अंदर टैक्स्ट डेटा को संग्रहीत करने में मदद करता है। यह उपयोगकर्ताओं को अधिक संगठित और आसानी से समझने वाले डेटा जारी रखने की अनुमति देता है।

## कैसे करें
कैसे कोडिंग उदाहरण और स्मापल आउटपुट के साथ "```Gleam ... ```" कोड ब्लॉक्स के भीतर सम्प्रदायन करें।

मान लें, आपको अपने Gleam प्रोग्राम में एक टेक्स्ट फ़ाइल बनानी है। उसके लिए, आपको एक नया फ़ाइल डिज़ाइन करने की आवश्यकता होगी। यदि आपका फ़ाइल "example.txt" है, तो आप "create_file" फ़ंक्शन का उपयोग कर सकते हैं:

```Gleam
import gleam/file.Create
import gleam/String

let file = Create.create_file("example.txt") 
|> case {
  Ok => "File created successfully"
  Error(err) => String.format("Failed to create file: {}", [err])
}
```

इस सरल उदाहरण के माध्यम से, आप अब अपनी फ़ाइल का प्रिंट भी कर सकते हैं:

```Gleam
import gleam/file/Write
import gleam/Process

Write.append(file, "Hello, World!") 
|> case {
  Ok => Process.terminate(255)
  Error(err) => String.format("Failed to write to file: {}", [err])
}
```

और अगर आप इस फ़ाइल को पढ़ना चाहते हैं, तो आप "read_file" फ़ंक्शन का उपयोग कर सकते हैं:

```Gleam
import gleam/file/Read

Read.read_file(file) 
|> case {
  Ok(contents) => contents
  Error(err) => String.format("Failed to read file: {}", [err])
}
```

और वहां आपको अपने फ़ाइल में लिखे गए "Hello, World!" को देख सकते हैं।

## गहराई में खोज
पाठ फ़ाइल लिखने के लिए गहराई की जानकारी।

पाठ फ़ा