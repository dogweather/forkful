---
title:    "Elm: वर्तमान तारीख प्राप्त करना"
keywords: ["Elm"]
---

{{< edit_this_page >}}

# Elm प्रोग्रामिंग के लिए वर्तमान तारीख प्राप्त करना क्यों जरूरी है?

अगर आप एक Elm डेवलपर हैं या इसमें रुचि रखते हैं, तो आपने शायद सोचा होगा कि वर्तमान तारीख प्राप्त करना क्यों जरूरी है। यह एक आपके प्रोग्राम में बहुत महत्वपूर्ण हो सकता है, क्योंकि जब आप अपने प्रोग्राम में वर्तमान तारीख का उपयोग करते हैं, तो आप अपने कोड को और अधिक उपयोगी बना सकते हैं।

## कैसे करें

कैसे करें का तरीका सीखने के लिए, हमें सबसे पहले Elm को इनस्टॉल करना होगा। आप [Elm's official website](https://guide.elm-lang.org/install.html) से इसे डाउनलोड और इनस्टॉल कर सकते हैं। एक बार इसे सफलतापूर्वक इनस्टॉल करने के बाद, हम इसका उपयोग करके वर्तमान तारीख को प्राप्त कर सकते हैं।

```Elm
import Time

-- वर्तमान तारीख को प्राप्त करने के लिए `now` फ़ंक्शन का इस्तेमाल करें
Time.now
```

आप यहां देख सकते हैं कि `now` फ़ंक्शन के उपयोग से हम वर्तमान तारीख को प्राप्त कर सकते हैं। इसके अलावा, आप `Time.utc` या `Time.local` का भी उपयोग कर सकते हैं जो आपको अन्य जगहों के समय को प्राप्त करने में मदद करते हैं।

## गहराई में जाएं

वर्तमान तारीख प्राप्त करने के लिए एलम इस्तेमाल करना बहुत आसान है। लेकिन दिनांक और समय को प्रबंधित करना थोड़ा संगठित हो सकता है। आप `Time` और `Date` मॉड्यूल