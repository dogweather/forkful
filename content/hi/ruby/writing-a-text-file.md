---
title:                "टेक्स्ट फाइल लिखना"
date:                  2024-01-19
simple_title:         "टेक्स्ट फाइल लिखना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
Ruby में टेक्स्ट फाइल लिखना मतलब डाटा को फाइल में सेव करना है. प्रोग्रामर इसे डाटा संरक्षित करने, लॉग फाइल बनाने, या यूज़र आउटपुट को सेव करने के लिए करते हैं.

## How to: (कैसे करें:)
```Ruby
# नई फाइल में लिखना
File.open('example.txt', 'w') do |file|
  file.puts "नमस्ते दुनिया!"
end

# फाइल खोलें और सामग्री देखें
puts File.read('example.txt')
```

नमस्ते दुनिया!

```Ruby
# मौजूदा फाइल में जोड़ना
File.open('example.txt', 'a') do |file|
  file.puts "रूबी से जुड़ी जानकारी."
end

# फाइल खोलें और सामग्री देखें
puts File.read('example.txt')
```

नमस्ते दुनिया!
रूबी से जुड़ी जानकारी.

## Deep Dive (गहराई में जानकारी)
पहले, प्रोग्रामर लो-लेवल फाइल हैंडलिंग APIs का इस्तेमाल करते थे, लेकिन रूबी ने यह काम आसान कर दिया है.  `File.open` के साथ 'w' मोड में फाइल को खोलकर, हम नए डाटा लिख सकते हैं, और 'a' मोड से मौजूदा फाइल में डाटा जोड़ सकते हैं. रूबी I/O के लिए बफरिंग जैसे अड्वांस फीचर्स भी हैंडल करती है.

## See Also (और जानकारी)
- रूबी डॉक्युमेंटेशन: [File Class](https://ruby-doc.org/core-3.0.0/File.html)
- "Learn Ruby the Hard Way" [रूबी सीखें](https://learnrubythehardway.org/book/)
