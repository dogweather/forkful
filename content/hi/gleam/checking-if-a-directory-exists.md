---
title:    "Gleam: क्या एक निर्देशिका मौजूद है"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# क्यों

यदि आप एक वेब डेवलपर हैं और अपने कोड में एक सुरक्षित तरीके से फ़ोल्डर की उपस्थिति को जांचना चाहते हैं, तो यह आपके लिए एक महत्वपूर्ण गतिविधि हो सकती है। यह आपके कोड को बग्स से बचाने और सुनिश्चित करने में मदद कर सकती है कि आपके सिस्टम के अन्दर निश्चित फ़ाइलें हैं या नहीं।

# कैसे करें

```Gleam
let directory_exists = fn(dir) {
  case std.fs.exists(dir) {
    True -> "डाइरेक्टरी मौजूद है।"  
    False -> "डाइरेक्टरी मौजूद नहीं है।"
  }
}

let dir_name = "/home/username/documents"
let result = directory_exists(dir_name)

```

आप ऊपर दिए गए कोड ब्लॉक में देख सकते हैं कि फ़ोल्डर की उपस्थिति को जांचने के लिए आपको `std.fs.exists()` फ़ंक्शन का उपयोग करना होगा। यह आपको `True` या `False` दोनों की वापसी करेगा, जोकि शेल का अंतर है। इसके बाद आप चाहे तो `if` कंडीशन का उपयोग कर सकते हैं और उसके आधार पर कोड को चला सकते हैं।

# गहराई में जाएं

फ़ोल्डर की उपस्थिति की जांच के लिए आप `std.fs.exists()` फ़ंक्शन के अलावा भी दूसरे तरीके भी उपयोग कर सकते हैं। आप `std.process.call()` फ़ंक्शन का भी उपयोग कर सकते हैं और शेल कमांड के माध्यम से फ़ोल्डर की उपस्थिति को जांच सकते हैं।

# देखें भी

- [Gleam डॉक्यूमेंटेशन](https://gleam.run/documentation/)
- [Gleam Github रेपो](https://github.com/gleam-lang/gleam)