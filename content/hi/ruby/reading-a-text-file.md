---
title:                "Ruby: एक टेक्स्ट फाइल को पढ़ना"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## क्यों

टेक्स्ट फ़ाइल पढ़ने के फायदे क्या हैं?

एक टेक्स्ट फ़ाइल में संग्रहित जानकारी को आसानी से पढ़ा जा सकता है, जो कि प्रोग्रामरों के लिए बहुत फायदेमंद हो सकता है। इसके अलावा, टेक्स्ट फ़ाइल बहुत सरल रूप से सामान्य टेक्स्ट ईडिटर में खोला जा सकता है। 

## कैसे करें

यदि आप रूबी में टेक्स्ट फ़ाइल पढ़ना सीखना चाहते हैं, तो निम्नलिखित कोड का उपयोग करके इसे प्रैक्टिस कर सकते हैं:

```Ruby
file = File.open("my_file.txt", "r")
contents = file.read
puts contents
```

इस उदाहरण में, हमने `my_file.txt` नाम की एक टेक्स्ट फ़ाइल खोली है और `file.read` का उपयोग करके उसकी सामग्री को पढ़ा है। अंत में, `puts` के माध्यम से उस सामग्री को कंसोल पर प्रिंट कर दिया है। 

## डीप डाइव

रूबी में टेक्स्ट फ़ाइल पढ़ने के बहुत सारे तरीके हैं और इसमें एक का उपयोग किया या एक से अधिक को अनुक्रमिक आदेश में पढ़ा जा सकता है। आगे बढ़ने से पहले, आपको फ़ाइल बंद करने के लिए `file.close` का उपयोग करना न भूलें। 

## देखें भी

- [Official Ruby Documentation on File I/O](https://ruby-doc.org/core-2.6.3/File.html)
- [Tutorialspoint - Reading a Text File in Ruby](https://www.tutorialspoint.com/ruby/ruby_input_output.htm)
- [Geeksforgeeks - Ruby File methods](https://www.geeksforgeeks.org/ruby-file-methods/)