---
title:                "Gleam: पैटर्न से मेल खाते अक्षरों को हटाना"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## क्यों

कभी-कभी हमें अपने कोड में से एक निशान जैसे कि मिटाने के लिए एक मैच पैटर्न के साथ पुराने अक्षरों को हटाने की जरूरत होती है। इस ब्लॉग पोस्ट में, हम इस विषय पर विस्तार से बात करेंगे और आपको Gleam का गुण दिखाएंगे कि इसका उपयोग करके हम कैसे आसानी से पैटर्न के हिसाब से अक्षर हटा सकते हैं।

## कैसे

```Gleam
let example_string = "Gleam Programming Language";
let pattern = "Gleam";
let result = string::replace(example_string, pattern, "")
```

इस कोड ब्लॉक में, हमने एक उदाहरण स्ट्रिंग बनाई है और फिर हमने उसमें दो चीजों को pass किया है - स्ट्रिंग जिस पर हम पैटर्न के हिसाब से अक्षर हटाना चाहते हैं और उस पैटर्न का नाम भी। और अंत में हमने `string::replace` फंक्शन का इस्तेमाल कर के उस स्ट्रिंग से उस पैटर्न के सारे अक्षरों को हटाया है।

```Gleam
// Output: " Programming Language"
```

इस उदाहरण में, हमने सफलतापूर्वक हमारे कोड से उन इकाईयों को हटाया है जो pattern string से मिलते हैं। सो, यहां हम जान सकते हैं कि हमारा कोड कैसे काम करता है व इसे उपयोगी बनाता है।

## गहराई में

`string::replace` फंक्शन गलतियों को सुधारने के लिए भी उपयोगी है। आपको ध्यान देना चाहिए कि अक्षरों को हटाने के लिए आपको पूरे पैटर्न के अनुसार करना होता है। एक और उदाहरण लेते हैं:

```Gleam
let example_string = "I love Gleam programming!"
let pattern = "i" // स्मृति करें कि हमने छोटा अक्षर दिया है
let result = string::replace