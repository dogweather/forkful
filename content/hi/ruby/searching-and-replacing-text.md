---
title:    "Ruby: पाठ की खोज और प्रत्याहार करना"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## क्यों

यह आमतौर पर आवश्यक नहीं है कि आप आपके कोड में Text को खोजें और उसे बदलें, लेकिन यह बहुत ही उपयोगी हो सकता है। यह आपको गलतियों को ढूंढने और सुधारने में मदद कर सकता है, साथ ही साथ आपके कोड को और सुरक्षित बनाने में भी आपकी सहायता कर सकता है।

## कैसे करें

आप Ruby का उपयोग करके आसानी से Text को खोजें और उसे बदल सकते हैं। आप जब भी किसी String के भीतर किसी विशिष्ट शब्द को खोजना चाहते हो, आप इस तरह से कोड कर सकते हैं:

```Ruby
text = "मेरा नाम रूबी है"
text.gsub!("रूबी", "हिंदी")
puts text

# Output: मेरा नाम हिंदी है
```

इस समाधान में, हमने String के `gsub!` मेथड का उपयोग किया है जो कि Text को खोजेगा और उसे बदलेगा। हमने `"रूबी"` को `"हिंदी"` से बदला है लेकिन आप जो चाहे उसे बदल सकते हैं। आप इस तरह भी regex का उपयोग कर सकते हैं या फिर आप `sub!` मेथड का भी उपयोग कर सकते हैं जो किसी भी शब्द को केवल एक बार ही बदलेगा।

## गहराई में

शायद आपको जानना हो कि परिवर्तन कैसे काम करता है। पहले, हम `gsub!` मेथड के भाग में एक regex या शब्द को खोजते हैं। यदि वह मिलता है, तो वह शब्द `!` मार्क के साथ बदल दिया जाएगा। यदि वह नहीं मिलता है, तो कोई परिवर्तन नहीं होगा और असली शब्द ही लौट जाएगा। उदाहरण के लिए:

```Ruby
text = "मेरा