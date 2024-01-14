---
title:    "Elm: दो तारीखों का तुलना करना"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## क्यों

क्या आपने कभी दो तारीखों को तुलना करने की कोशिश की है? शायद आप एक स्प्रेडशीट बनाने के लिए या बस दो तारीखों के बीच समय अंतर को जानने के लिए? जो भी हो, इल्म में तारीखों को तुलना करना बहुत आसान है और यह आपको अपने कोड को दोनों के बीच बहुत स्पष्ट और सुव्यवस्थित बनाने में मदद कर सकता है।

## कैसे करें

इल्म में दो तारीखों के बीच समय अंतर को प्राप्त करने के लिए हमें सिंपल फ़ंक्शन `Time.diff` का उपयोग करना होगा। इसका निर्देश निम्नलिखित है:

```Elm
diff : Time -> Time -> Time
```

जैसा कि आप ऊपर देख सकते हैं, इस फ़ंक्शन में दो तारीख प्रारंभ करने के लिए दो ऑब्जेक्ट के लिए समय को लेना होगा। इसकी उदाहरण फ़ॉर्मेट निम्नलिखित है:

```Elm
diff (Time.millisToUtc 0) (Time.millisToUtc 1000)
```

इस समय अंतर की इकाई न्यूनतम समय होगा। यदि आप इस समय अंतर को प्राप्त करना चाहते हैं, तो आपको इसे एक `Int` के रूप में पार्स करना होगा।

```Elm
diff (Time.millisToUtc 0) (Time.millisToUtc 1000) |> Time.inMilliseconds
```

इसका परिणाम 1000 मिलिसेकंड होगा। आप इससे फ़ॉर्मेट भी बदल सकते हैं जैसे कि `inDays` या `inMinutes`.

## गहराई में खोजें

इल्म में तारीखों को तुलना करना आसान है लेकिन कुछ गहराई में जाने से आप इसमें अधिक सुविधाजनक काम कर सकते हैं। `Time` मॉड्यूल में एक और उपयोग