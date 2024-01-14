---
title:    "Gleam: लोअर केस में एक स्ट्रिंग कन्वर्शन"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्यों

कभी-कभी हमारे द्वारा दर्ज किए गए स्ट्रिंग में अक्षरों के स्थानांतरण के लिए स्पष्टता और संभावितता का उपयोग करना आवश्यक होता है। इसलिए, स्ट्रिंग को lowercase में रूपांतरित करना एक आवश्यक कार्य हो सकता है।

## कैसे करें

```Gleam
string = "HELLO WORLD"
new_string = string |> String.to_lower
```

आपको केवल ऊपर दिए गए उदाहरण के अनुसार `String.to_lower` फंक्शन को स्ट्रिंग पर लागू करना होगा। उपरोक्त उदाहरण के लिए आपको निम्न आउटपुट मिलेगा:

```Gleam
"hello world"
```

## गहराई में जाएं

स्ट्रिंग को lowercase में रूपांतरित करना एक सरल कार्य हो सकता है, लेकिन इसके पीछे का गुच्छ है कि स्ट्रिंग मैनिपुलेशन क्यों भी आपके लिए महत्वपूर्ण है। आपको अपने कोड में स्ट्रिंग मैनिपुलेशन के युक्तियों को जानना चाहिए ताकि आप अपने स्क्रिप्टों को और अधिक अनुकूलित बना सकें।

## देखें भी

- [गाइड: Gleam में उपलब्ध डेटा टाइप ](https://docs.gleam.run/types/)
- [डॉक: Gleam में उपलब्ध String मोड्यूल](https://docs.gleam.run/std/string.html#to_lower)