---
title:                "पाठ की खोज और प्रतिस्थापन"
html_title:           "Bash: पाठ की खोज और प्रतिस्थापन"
simple_title:         "पाठ की खोज और प्रतिस्थापन"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## क्या & क्यों?

खोजना और पाठ को बदलना मतलब है कि हमें दिए गए टेक्स्ट के भीतर से एक विशेष टेक्स्ट को ढूंढना होता है ঔर फिर उसे किसी दूसरे टेक्स्ट से बदल देना। प्रोग्रामर्स इसे इसलिए करते हैं क्योंकि यदि किसी बड़े डाटा सेट के भीतर हमें कुछ बदलना है, तो यूजर्स को आसानी होती है।

## कैसे करें:

```Haskell
import Data.String.Utils

main = putStrLn (replace "खोजने वाला टेक्स्ट" "बदलने वाला टेक्स्ट" "मूल टेक्स्ट")
```

उदाहरण के लिए, यदि हम चाहते हैं कि 'केला' को 'सेव' में बदल दें, हम तब इसे इस तरह करेंगे:

```Haskell
main = putStrLn (replace "केला" "सेव" "मैं केला खा रहा हूँ")
```

साम्पल आउटपुट:

```
मैं सेव खा रहा हूं
```

## गहरा डाइव:

### इतिहास
हास्केल में, पाठ को खोजने और बदलने की कार्यक्षमता पहली बार 1990 में जोड़ी गई थी, जब हास्केल का पहला स्थिर संस्करण जारी हुआ था।

### विकल्प
वेर्बोस ओपरेशन के बजाय, हम `Data.Text` पैकेज का उपयोग करके भी इसे प्रदान कर सकते हैं।

### वास्तविकता
पाठ को खोजने और बदलने के लिए हास्केल में एक तत्व का उपयोग किया जा रहा है, जिसे 'लेजी एवाल्यूएशन' कहते हैं।

## और भी देखें:

- हास्केल डॉक्यूमेंटेशन: https://www.haskell.org/documentation/
- पाठ को खोजविद्धार्थ: https://hackage.haskell.org/package/regex-posix
- लेजी एवाल्यूएशन: https://wiki.haskell.org/Lazy_evaluation
- `Data.Text` पैकेज: https://hackage.haskell.org/package/text