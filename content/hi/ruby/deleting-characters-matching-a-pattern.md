---
title:                "Ruby: पैटर्न से मेल खाते अक्षर हटाना"
simple_title:         "पैटर्न से मेल खाते अक्षर हटाना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## क्यों

आखिरकार, हम जब कोई टेक्स्ट डाटा को इस्तेमाल करते हैं, तो हमें अक्सर ऐसे स्थिति से गुजरना पड़ता है जब हमें निश्चित पैटर्न के अनुसार अक्षरों को हटाना होता है। इसलिए हम Ruby में इस विषय पर काम करते हैं।

## कैसे करें

```Ruby
str = "coding in Ruby is fun!"
puts str.gsub(/i/, "") 
# Output: codng n Ruby s fun!
```

यहां हमने एक टेक्स्ट स्ट्रिंग बनाई है जिसमें "i" अक्षरों को हटाने के लिए `gsub` फंक्शन इस्तेमाल किया गया है। इस उदाहरण से आपको स्पष्ट रूप से समझ आ गया होगा कि पैटर्न मैचिंग के दौरान कैसे अक्षरों को हटाया जा सकता है।

## गहराई में जाएं

इस विषय को गहराई से समझने के लिए हमें रिजेक्ट इक्वलेंट `reject_equal?` मेथड की जांच करनी होगी। यह मैच केस स्टडी के लिए आवश्यक है क्योंकि इसमें हमें पैटर्न को ध्यान में रखने और उसको पूरा करने के लिए स्लीक समाधान ढूंढने की जरूरत होती है।

### प्रयोग मिलान

```Ruby
def reject_equal?(pattern)
    self.each do |char|
        return false if char == pattern
    end
    return true
end

str = "coding in Ruby is fun!"
puts str.chars.reject {|char| char == "i"}.join
# Output: codng n Ruby s fun!
```

यहां, हमने `reject_equal?` मेथड में स्ट्रिंग चर्नों को पास करने तथा उन्हें जोड़े बिना फिल्टर किया है। इसमें हमने फिल्टर करने के लिए लूप का उपयोग किया है ताकि हम प्रत्येक चर को पैटर्न के समान या बराबर तथा उसके अनुसार ग्रुप छोड़ सकें।

## देखें भी