---
title:                "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"
aliases: - /hi/ruby/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:39:27.700061-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? / क्या और क्यों?
स्ट्रिंग को लोअर केस में बदलना मतलब है सभी अक्षरों को छोटे (lowercase) अक्षरों में परिवर्तित करना। प्रोग्रामर इसे करते हैं ताकि डेटा को सामंजस्यपूर्ण और सेंसिटिविटी-मुक्त (case-insensitive) बनाया जा सके।

## How to: / कैसे करें:
```ruby
# एक साधारण उदाहरण
phrase = "नमस्ते, आप कैसे हैं?"
puts phrase.downcase
# आउटपुट होगा: "नमस्ते, आप कैसे हैं?"

# अंग्रेज़ी अक्षरों के साथ
english_phrase = "Hello, How Are You?"
puts english_phrase.downcase
# आउटपुट होगा: "hello, how are you?"
```

## Deep Dive / गहराई से जानकारी:
पुराने ज़माने में, कंप्यूटर अक्षरों को महज़ नंबरों के रूप में पहचानते थे। लेकिन, इंसानों को तो अक्षरों की ज़रूरत होती है, तो फिर 'Character Encoding Standards' आए। ASCII एक मानक है जो सीधे सीधे कहे तो अंग्रेज़ी अक्षरों के लिए है। लेकिन यूनिकोड, जो की एक बड़ा मानक है, विश्व की विभिन्न भाषाओं के लिए है।

Ruby में `downcase` मेथड यूनिकोड का समर्थन करता है, इसलिए यह अलग अलग लैंग्वेज के केस ट्रांसफॉर्मेशन के लिए उपयोगी है। हालांकि, `.downcase` के विकल्प के रूप में `.downcase!` और `.downcase` के अन्य समान विधियां भी हैं, जिनमें `.swapcase` और `capitalize` शामिल हैं जो केस को विशिष्ट तरीकों में बदलती हैं।

Ruby डेवलपर को यह ध्यान रखना होगा कि भिन्न भाषाएँ और उनके केस नियम भिन्न होते हैं। कुछ अक्षरों का लोअरकेस परिवर्तन सीधा नहीं होता। `String#downcase` को समझने के लिए यह जानना महत्वपूर्ण है कि यह Unicode Character Database (UCD) का अनुसरण करता है।

## See Also / और भी जानकारी:
- Ruby Documentation on String: https://ruby-doc.org/core-2.7.1/String.html#method-i-downcase
- Unicode Character Database: http://www.unicode.org/reports/tr44/
- Article on Case Mapping: https://www.unicode.org/reports/tr21/tr21-5.html

ये लिंक्स आपको Ruby और यूनिकोड के मानकों के बारे में और जानकारी देंगे, ताकि आपको स्ट्रिंग के केस-परिवर्तन में महारत हासिल हो सके।
