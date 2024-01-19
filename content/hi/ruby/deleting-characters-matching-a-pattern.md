---
title:                "पैटर्न से मिलते जुलते वर्णों को हटाना"
html_title:           "Elixir: पैटर्न से मिलते जुलते वर्णों को हटाना"
simple_title:         "पैटर्न से मिलते जुलते वर्णों को हटाना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

पैटर्न से मेल खाने वाले कैरेक्टर्स को डिलीट करना का मतलब है की उस स्ट्रिंग को हटाना जो एक निर्धारित पैटर्न से मिलती है। प्रोग्रामर्स करते हैं इसे ताकि वे ऐसे डेटा को हटा सकें जो उन्हें नहीं चाहिए, या जो उनको डिस्टर्ब करता है।

## ऐसे कैसे:

Ruby में, आप `delete` मेथड का उपयोग करके कैरेक्टर्स को हटा सकते हैं:

```ruby
# कोड
str = "Hello, Ruby!"
str.delete!("o")

# आउटपुट
=> "Hell, Ruby!"
```
इस उदाहरण में, हमने 'o' कैरेक्टर को 'str' से हटा दिया।

## गहरा डाइव:

`delete` मेथड Ruby 1.9.3 से शामिल हुआ था। इसके अलावा, आप `gsub` मेथड का भी उपयोग कर सकते हैं, जो 'delete' की तरह ही काम करता है, लेकिन यह एक पैटर्न के साथ मार्च करता है:

```ruby
# कोड
str = "Hello, Ruby!"
str.gsub!(/o/, '')

# आउटपुट
=> "Hell, Ruby!"
```

`gsub` का उपयोग करने से आपके पास कुछ और फ्लेक्सिबिलिटी होती है, लेकिन अगर आप सिंपल कैसेस के लिए तलाश कर रहे हैं तो `delete` मेथड अच्छा विकल्प हो सकता है।

## देखें भी:

- [Ruby's String Documentation](https://ruby-doc.org/core-2.7.1/String.html) - नेटिव स्ट्रिंग मेथड्स के विवरण के लिए।
- [Regular Expressions in Ruby](https://www.rubyguides.com/2015/06/ruby-regex/) - रेगेक्स्प की अधिक जानकारी के लिए।