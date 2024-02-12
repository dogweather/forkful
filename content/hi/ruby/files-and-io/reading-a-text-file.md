---
title:                "टेक्स्ट फ़ाइल पढ़ना"
aliases:
- /hi/ruby/reading-a-text-file.md
date:                  2024-01-20T17:55:42.744115-07:00
model:                 gpt-4-1106-preview
simple_title:         "टेक्स्ट फ़ाइल पढ़ना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

टेक्स्ट फाइल पढ़ना यानी फाइल से डाटा को रीड करना। प्रोग्राम्मेर्स करते क्यों हैं? सिंपल, डाटा प्रॉसेस करने, यूजर इनपुट समझने, और सेटिंग्स लोड करने के लिए।

## How to: (कैसे करें:)

```Ruby
# एक टेक्स्ट फाइल को पढ़ने का सबसे आसान तरीका
File.open("example.txt").each do |line|
  puts line
end

# पूरी फाइल को एक स्ट्रिंग के रूप में पढ़ना
text = File.read("example.txt")
puts text

# फाइल से लाइन बाय लाइन पढ़ना और उन्हें एक एर्रे में स्टोर करना
lines = File.readlines("example.txt")
puts lines
```

## Deep Dive (गहराई से समझिए:)

रूबी में फाइल पढ़ना इतना सिम्पल है कि बेसिक्स याद रखना आसान है। पहले के जमाने में, जब रूबी नहीं थी, C और दूसरी लैंग्वेज में फाइल ऑपरेशंस थोड़े कॉम्प्लिकेटेड थे। लेकिन रूबी ने यह प्रोसेस आसान बनाया।

रूबी में `File.open` एक ब्लॉक के साथ इस्तेमाल होता है तो यह फाइल को आपके ब्लॉक के बाद ऑटोमैटिकली बंद भी कर देता है, जिससे मेमोरी लीक नहीं होती। आप `File.read` या `File.readlines` का इस्तेमाल करके आसानी से पूरी फाइल या लाइन्स को एक बार में पढ़ सकते हैं। लेकिन बड़ी फाइल्स के लिए, लाइन बाय लाइन पढ़ना बेहतर हो सकता है ताकि मेमोरी पर ज्यादा लोड ना पड़े।

## See Also (यह भी देखें:)

- [Ruby Documentation for the File Class](https://ruby-doc.org/core/File.html)
- [Ruby IO Documentation](https://ruby-doc.org/core/IO.html)
