---
title:    "Rust: सबस्ट्रिंग्स निकालना"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## क्यों

सबसे पहले, हम लोग программировँग में तरह-तरह के तारीकों से डेटा को हैंडल करते हैं। स्ट्रिंग्स या वर्णश्रृंखलाएं भी उनमें से एक होती हैं। वर्णश्रृंखलाओं को समाप्तक स्ट्रिंग्स में छोटे भागों में विभाजित करने के लिए हम उन भागों को निकालने की जरूरत हो सकती है। इसलिए, आज हम देखेंगे कि रस्ट में वर्णश्रृंखलाओं को कैसे निकाला जा सकता है।

## कैसे निकालें

वर्णश्रृंखलाओं को निकालने के लिए आवश्यक फ़ंक्शन्स हमें स्ट्रिंग स्लाइसिंग और उसमें उपयोग करने का तरीका देते हैं। निम्न उदाहरण में, हम हैंस्ग्रीन फ़ंक्शन (substring function) का उपयोग करके स्ट्रिंग्स से वर्णश्रृंखलाएं निकालेंगे।

```Rust
let s = "हम लोग रस्ट सीख रहे हैं।";
let substring = &s[4..9];
println!("{}", substring);
```

उपरोक्त कोड ब्लॉक में, हमने अपनी स्ट्रिंग "हम लोग रस्ट सीख रहे हैं।" से उस किसी भी भाग को निकाला है जो अंक 4 से शुरू होता है और अंक 9 तक का होता है। इस प्रकार से, हम उस भाग को निकाल सकते हैं जो हमें आवश्यक होता है।

## डीप डाइव

वर्णश्रृंखलाओं को निकालना श्रेष्ठ तरीका उनके बीच अंतर को ज्ञात करना हो सकता है। इससे हमें किसी भी अनुवांशिक डेटा को आसानी से प्राप्त करने में मदद मिल सकती है। रस्ट में वर्णश्रृंखलाओं को न