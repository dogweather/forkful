---
title:                "Gleam: स्ट्रिंग को लोअर केस में बदलना"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# क्यों

क्या आप अपने Gleam कोड में स्ट्रिंग को लोअर केस में बदलने को जानना चाहते हैं? यह बहुत ही उपयोगी हो सकता है क्योंकि इससे आपको स्ट्रिंग को मानकीकृत करने में मदद हो सकती है और आपके कोड में संघटना को बढ़ावा दे सकती है।

# कैसे करें

आइए देखें कि स्ट्रिंग को लोअर केस में कैसे बदला जा सकता है ग्लीम में: 

```Gleam
let string = "Hello World"
let lower_case = String.to_lower(string)
```

यहां हमने `String.to_lower` फंक्शन का उपयोग किया है जो दिए गए स्ट्रिंग को लोअर केस में बदल देता है। अब हम `lower_case` वेरिएबल में इस बदलाव को स्टोर कर सकते हैं और उसे उपयोग कर सकते हैं। चलिए इसका आउटपुट देख लेते हैं: 

```Gleam
IO.print(lower_case)
```

आउटपुट: "hello world"

# गहराई में जाएं

जबकि यह एक सरल प्रक्रिया है, आपको यह जानना भी जरूरी है कि स्ट्रिंग को कैसे लोअर केस में बदला जाता है। ग्लीम में, स्ट्रिंग डेटा टाइप `String` नहीं है, बल्कि `char_list` है। यहां हम दो चीज़ों को काम में लेना चाहेंगे: 

- स्ट्रिंग को `char_list` में बदलना 
- चर लिस्ट को स्ट्रिंग में बदलना 

आइए परेशानी को शुरू करते हैं। यहां हम दो स्ट्रिंग को लोअर केस में बदलते हैं: 

```Gleam
let string1 = "Hello World"
let string2 = "Gleam Programming"
let lower_string1 = String.to_lower(string1)
let lower_string2 = String.to_lower(string2)
```

अब हम दोनों स्ट्रिंग को दूसरे चर लिस्ट में बदलेंगे और फिर इसे स्ट्रिंग म