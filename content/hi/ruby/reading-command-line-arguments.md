---
title:                "Ruby: कम्प्यूटर प्रोग्रामिंग पर लेख: कमांड लाइन आर्ग्यूमेंट को पढ़ना।"
simple_title:         "कम्प्यूटर प्रोग्रामिंग पर लेख: कमांड लाइन आर्ग्यूमेंट को पढ़ना।"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## क्यों

कमांड लाइन आर्ग्यूमेंट पढ़ने में लोग क्यों रूचि रखते हैं, इसके सम्बंध में एक या दो सेंटेंस का विवरण।

## कैसे करें

```ruby
# पहला तरीका
ARGV.each do |arg|
  puts arg
end

# दूसरा तरीका
puts ARGV
```

चलिए Ruby में कमांड लाइन आर्ग्यूमेंट्स पढ़ना सीखें। पहले तरीके में, हम `ARGV` के `each` मेथड का इस्तेमाल करके, हर आर्ग्यूमेंट को एक-एक करके प्रिंट कर सकते हैं। दूसरे तरीके में, हम सीधे `ARGV` को प्रिंट कर सकते हैं। क्योंकि `puts` ऑटोमेटिकली हर आर्ग्यूमेंट को स्पेस के साथ प्रिंट कर देता है।

## गहराई से जानिए

कमांड लाइन आर्ग्यूमेंट्स पढ़ने के बारे में थोड़ा गहराई से जानने के लिए, हमें दो चीजें समझनी होंगी। पहली बात है कि, `ARGV` एक array है, जिसमें प्रोग्राम के लौचर अग्रिम में दिए गए सभी आर्ग्यूमेंट्स होते हैं। दूसरी बात है कि, हम इस array का इस्तेमाल करके आर्ग्यूमेंट्स के साथ कुछ काम कर सकते हैं। जैसे कि, अगर हमें किसी आर्ग्यूमेंट को नया नाम देना हो, तो हम इस array का इस्तेमाल कर सकते हैं।

## इससे सम्बंधित

### ऑनलाइन रवी कोर्स

[Codecademy - Reading Command Line Arguments in Ruby](https://www.codecademy.com/learn/ruby/modules/learn-ruby-control-flow/u/lessons/introduction-to-control-flow/exercises/reading-command-line-arguments)

### रुबी डॉक्यूमेंटेशन

[RubyDocs - ARGF](https://ruby-doc.org/core-2.6.3/ARGF.html)

[RubyDocs - ARGV](https://ruby-doc.org/core-2.6.3/ARGV.html)

## देखकर मिलेगा

[Codecademy - Ruby ट्यूटोरियल