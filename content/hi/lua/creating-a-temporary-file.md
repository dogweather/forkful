---
title:                "कम्प्यूटर प्रोग्रामिंग में अस्थायी फ़ाइल बनाना।"
html_title:           "Lua: कम्प्यूटर प्रोग्रामिंग में अस्थायी फ़ाइल बनाना।"
simple_title:         "कम्प्यूटर प्रोग्रामिंग में अस्थायी फ़ाइल बनाना।"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Lua में अस्थायी फ़ाइल कैसे बनाएं?

## क्या है और क्यों करें?
किसी भी लेखक या कोडर के लिए धाराप्रवाह में अस्थायी फ़ाइल एक उपयोगी उपकरण है। यह एक चोटी-मोटी फ़ाइल होती है जो कुछ समय के लिए सिस्टम में सिर्फ़ एक बार उपलब्ध रहती है, और इसे उसके उपयोग के बाद ऑटोमेटिक रूप से हटा दिया जाता है। ये अस्थायी फ़ाइलें विभिन्न कार्यों के दौरान पायी जाती हैं, जैसे डाटा प्रवाह का स्टोरेज करना, प्रोग्राम के दौरान प्रतिक्रिया देना, या बिना खतरे के कोड समाप्त करना।

## कैसे करें?
अस्थायी फ़ाइल सरल और स्ट्रेटफार्ड होती है, और इसे Lua में बनाना भी बहुत आसान है। यहां हम इसे बनाने का एक आसान तरीका देखेंगे:

```lua
-- अस्थायी फ़ाइल बनाएं
local temp_file = io.tmpfile()

-- सामग्री लिखें
temp_file:write("मैं एक अस्थायी फ़ाइल हूँ!")

-- वापसगई करें और स्क्रीन पर प्रिंट करें
temp_file:seek("set")
print(temp_file:read("*a"))

-- फ़ाइल हटा दें
temp_file:close()
```

यह अस्थायी फ़ाइल को स्क्रीन पर प्रिंट करेगा:

```
मैं एक अस्थायी फ़ाइल हूँ!
```

इसके अलावा आप अस्थायी फ़ाइल को साथ साथ पढ़ने और लिखने भी कर सकते हैं।

## गहरी खोज
अस्थायी फ़ाइल का लगभग 40 साल पुराना इतिहास है। पहले यह UNIX में ही प्रोग्राम लोगों द्वारा इस्तेमाल किया जाता था, परन्तु आज इससे लाखों कोडर्स और लेखकों द्वारा प्रोग्राम में इस्तेमाल किया जाता है।

खासीयत की दृष्टि से, अस्थायी फ़ाइल के दो प्रमुख विकल्प हैं - ऐसी फ़ाइलें जो टेम्पोररी वर्कफ़ोल्डर में होंगी और ऐसी जो सिस्टम के दौरान होंगी। दोनों के बीच का अंतर यह है कि सिस्टम वाली फ़ाइलें सामान्य तौर पर अपने आप हट जाती हैं। इसलिए, पर्याप्त स्टोरेज उपलब्ध होने के लिए यह विकल्प अधिक अनुकूल हो सकता है।

अस्थायी फ़ाइल को बनाने के लिए भी कई अन्य तरीके हैं, जैसे आप आपके सिस्टम में एक अस्थायी फ़ाइल बनाने को भीसकते हैं या आप अ