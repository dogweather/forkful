---
title:                "एक अस्थायी फ़ाइल बनाना"
html_title:           "Arduino: एक अस्थायी फ़ाइल बनाना"
simple_title:         "एक अस्थायी फ़ाइल बनाना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Ruby के साथ अस्थायी फ़ाइलें कैसे बनाएं

## क्या और क्यों?

अस्थायी फ़ाइलें, जैसा कि नाम सुझाता हैं, कुछ समय के लिए डेटा स्टोर करने के लिए बनाई जाती हैं। ये फ़ाइलें तब उपयोगी होती हैं, जब आपको कुछ डेटा तब तक स्टोर करके रखना होता है, जब तक कि कुछ निश्चित क्रियाएँ पूरी नहीं हो जाती।

## कैसे करें:

Ruby में `Tempfile` लाइब्रेरी का उपयोग करके अस्थायी फ़ाइलें बनाई जा सकती हैं। 

```Ruby
require 'tempfile'

# अस्थायी फ़ाइल बनाने के लिए
file = Tempfile.new('hello')
puts file.path      # => "/tmp/hello... . .. .. .3.8" (रैंडम नाम)
file.write("world") # फ़ाइल में लिखने के लिए
file.rewind         # फ़ाइल के शुरू में पुनः जाने के लिए

puts file.read      # => "world"
file.close          # फ़ाइल को बंद करने के लिए
```

## गहरी छानबिन

अस्थायी फ़ाइलें अस्थायी स्थोरेज़ समाधान के रूप में लंबे समय से उपयोग हो रही हैं। यह एक सुरक्षित तरीका प्रदान करता है डेटा स्टोर करने का जो बाद में हटा दिया जाता है। Ruby में, `Tempfile` लाइब्रेरी 'tmpdir' लाइब्ररी का उपयोग करती है जो अगर आप चाहते हैं, तो अलग से भी उपयोग कर सकते हैं।

## अन्य जानकारी के लिए:

1. [Ruby Tempfile Documentation](https://ruby-doc.org/stdlib-3.0.1/libdoc/tempfile/rdoc/Tempfile.html)
2. [Ruby TmpDir Documentation](https://ruby-doc.org/stdlib-3.0.1/libdoc/tmpdir/rdoc/Tmpdir.html)