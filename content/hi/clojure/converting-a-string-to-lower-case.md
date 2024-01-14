---
title:    "Clojure: स्ट्रिंग को लोअर केस में रूपांतरण"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्यों

कोई भी शब्द को छोटे अक्षरों में बदलने के संबंध में तो जानते ही होंगे। परंतु क्लोजर में भी यह चीज़ बहुत साधारण हो सकती है। इस आलेख में हम देखेंगे कि क्लोजर में शब्द को छोटे अक्षरों में बदलना कितना आसन है।

## कैसे करें

आप क्लोजर में शब्द को छोटे अक्षरों में बदलने के लिए निम्नानुसार कोड इस्तेमाल कर सकते हैं: 

```Clojure
(defn convert-to-lower [string]
    (clojure.string/lower-case string))
```

इसका उपयोग करके आप आसानी से किसी भी शब्द को छोटे अक्षरों में बदल सकते हैं। नीचे दिए गए उदाहरण में हमने दो शब्दों को छोटे अक्षरों में बदला है और उनका आउटपुट प्रदर्शित किया है। 

```Clojure 
(convert-to-lower "क्लोजर") ; क्लोजर
(convert-to-lower "हमारा साथी") ; हमारा साथी
```

## गहराई में जाएं

यदि आप क्लोजर का एक दीवाना हैं और इसकी गहराई में जाना चाहते हैं तो आप शब्द को छोटे अक्षरों में बदलने का इनपुट कोड कैसे करते हैं, उसके बारे में भी जान सकते हैं। यह कोड मुख्य रूप से इस लाइन पर निर्धारित है: 

```Clojure
(str/lower-case string)
```

इसके अलावा आप इस फ़ंक्शन का भी इस्तेमाल कर सकते हैं: 

```Clojure
(clojure.string/lower-case string)
```

## देखें भी

- [आधिकारिक Clojure डॉक्यूमेंटेशन] (https://clojure.org/guides/cheatsheet)
- [Clojure में शब्दों को छोटा करना] (https://www.baeldung.com/clojure/string-lowercase)