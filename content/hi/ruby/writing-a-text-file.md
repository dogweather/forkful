---
title:                "Ruby: टेक्स्ट फाइल लेखन"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## क्यों

एक पाठ फाइल लिखने में क्यों शामिल होना चाहिए? एक पाठ फाइल आपके दस्तावेज़ों, टेक्स्ट संदेशों, या कोड का सबसे सादा रूप है। यह आपको अपने काम को संगठित रखने और साझा करने में मदद करता है।

## कैसे करें

```ruby
# नयी पाठ फाइल खोलें
file = File.new("new_file.txt", "w")

# टेक्स्ट लिखें
file.puts("हैलो हिन्दी रीडर्स!")

# फाइल बन्द करें
file.close

# फाइल से टेक्स्ट पढ़ें
file = File.open("new_file.txt", "r")

# प्रिंट करें
puts file.gets
```

आप ऊपर दिए गए उदाहरण के साथ एक पाठ फाइल खोल सकते हैं, टेक्स्ट लिख सकते हैं, उसे बंद कर सकते हैं, और अंत में उसमे से टेक्स्ट पढ़ सकते हैं। आप और भी उदाहरण देखने के लिए यहां जा सकते हैं: [Ruby फाइल से टेक्स्ट पढ़ें](https://www.geeksforgeeks.org/ruby-file-gets.html) और [Ruby में फाइल और फ़ाइल आपरेशन](https://www.tutorialspoint.com/ruby/ruby_input_output.htm)

## गहराई में

एक पाठ फाइल को अधिक समारोहपूर्ण बनाने के लिए, आप एक निश्चित फॉर्मेट में टेक्स्ट लिख सकते हैं, जैसे कि CSV या JSON। आप इसमें अलग-अलग तरह की डेटा भी जोड़ सकते हैं, जैसे कि प्रकार, स्थान, या समय। अधिक जानकारी के लिए, [Ruby में CSV फ़ाइल कैसे लिखें](https://www.rubyguides.com/2018/10/ruby-csv-library/) और [JSON क्या है और Ruby में इसका कैसे प्रयोग करे](https://ruby-doc.org/stdlib-2.6.1/libdoc/json/rdoc/JSON.html) पढ़ें।

## देखें भी

* [Ruby में फाइल और फ़ाइल आपरेशन](https://www.tutorialspoint.com/ruby