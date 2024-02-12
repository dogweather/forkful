---
title:                "स्ट्रिंग से उद्धरण चिह्न हटाना"
aliases:
- /hi/ruby/removing-quotes-from-a-string/
date:                  2024-01-26T03:43:58.939587-07:00
model:                 gpt-4-0125-preview
simple_title:         "स्ट्रिंग से उद्धरण चिह्न हटाना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
एक स्ट्रिंग से उद्धरण चिह्नों को हटाने का अर्थ है उन डबल या सिंगल उद्धरण चिह्नों को निकालना जिनसे टेक्स्ट मान घिरे होते हैं। प्रोग्रामर्स अक्सर इसे यूज़र इनपुट को साफ़ करने, डेटा प्रोसेसिंग में एकसमानता सुनिश्चित करने, या डेटा को ऐसी प्रणालियों के लिए तैयार करने के लिए करते हैं जो इन अतिरिक्त चरित्रों से भ्रमित हो सकती हैं।

## कैसे:
रूबी के पास उन परेशान करने वाले उद्धरण चिह्नों को निकालने के लिए कुछ अच्छी तरकीबें हैं। आप `gsub` या `delete` मेथड्स का उपयोग करके काम कर सकते हैं। यहाँ कुछ कोड दिया गया है:

```ruby
# डबल और सिंगल कोट्स हटाने के लिए gsub का उपयोग करना
quoted_string = "\"Say 'hello' to my little friend!\""
unquoted_string = quoted_string.gsub(/'|"/, '')
puts unquoted_string 
# आउटपुट: Say hello to my little friend!

# अगर आप जानते हैं कि आप केवल एक प्रकार के कोट से निपटेंगे
single_quoted_string = "'Stay a while and listen!'"
clean_string = single_quoted_string.delete("'")
puts clean_string 
# आउटपुट: Stay a while and listen!
```

## गहराई से जानकारी
कोट्स का इतिहास प्रोग्रामिंग के सबसे पहले दिनों में लौटता है, जहाँ वे अक्सर स्ट्रिंग डेलिमिटर्स के रूप में काम करते थे। आज भी, जैसे तब, आप पाएंगे कि जब वे जरूरी नहीं होते हैं या जब वे डेटा संग्रहण और संचालन के साथ हस्तक्षेप कर सकते हैं तब आपको इन उद्धरण चरित्रों को हटाने की आवश्यकता हो सकती है।

हमने `gsub` और `delete` के बारे में बात की है लेकिन अन्य मेथड्स भी हैं, जैसे `tr` या `tr_s`, जो आपको थोड़ा और नियंत्रण देते हैं या कुछ अलग उपयोग के मामलों को संभाल सकते हैं:

```ruby
# tr भी उद्धरण चिन्हों को हटा सकता है
double_quoted_string = "\"Do or do not, there is no try.\""
clean_string = double_quoted_string.tr('\"', '')
puts clean_string 
# आउटपुट: Do or do not, there is no try.
```

याद रखें, इन प्रत्येक मेथड्स का उपयोग-केस होता है। `gsub` जब आप जटिल पैटर्न या कई प्रतिस्थापनों के साथ काम कर रहे होते हैं तब अधिक शक्तिशाली होता है। `delete` और `tr` सरल, सीधे चरित्र हटाने के लिए बेहतरीन काम करते हैं।

## देखें भी
अधिक पठन के लिए, और इन मेथड्स को बड़े कोडबेस के भीतर कार्रवाई में देखने के लिए, देखें:
- रूबी दस्तावेज़ीकरण [String#gsub](https://ruby-doc.org/core-3.1.2/String.html#method-i-gsub), [String#delete](https://ruby-doc.org/core-3.1.2/String.html#method-i-delete), और [String#tr](https://ruby-doc.org/core-3.1.2/String.html#method-i-tr) के लिए।
- रूबी मॉन्स्टाज़ का बेहतरीन [स्ट्रिंग व्यायाम सेट](http://ruby-for-beginners.rubymonstas.org/built_in_classes/strings.html) जिसमें कोट्स के साथ काम करना शामिल है।
- स्ट्रिंग मेनिपुलेशन पर स्टैक ओवरफ्लो चर्चाएँ [स्ट्रिंग मेनिपुलेशन](https://stackoverflow.com/search?q=ruby+remove+quotes+from+string) आपको साथी रूबीइस्ट्स से वास्तविक दुनिया की समस्याओं और समाधान देती हैं।
