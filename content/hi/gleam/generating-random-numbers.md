---
title:                "यादृच्छिक संख्याओं का निर्माण"
html_title:           "Clojure: यादृच्छिक संख्याओं का निर्माण"
simple_title:         "यादृच्छिक संख्याओं का निर्माण"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)

अनुगोल (रैंडम) संख्याओं का उत्पादन क्या होता है? इसका विचार एक क्रम में यादृच्छिकता जोड़ने के रूप में किया जा सकता है। प्रोग्रामर्स इसे करते हैं क्योंकि यह लचीलापन और अनपेक्षित परिणाम प्रदान करता है, जो नियमित रूप से छोटी संख्याओं के लिए उपयोगी होता है।

## कैसे: (How to:)

Gleam में, गलीम सीड लाइब्रेरी का उपयोग करके यादृच्छिक संख्या उत्पन्न की जा सकती है। यह तरीका नीचे दिखाया गया है:

```Gleam
let rng = Random.new(:os.system_time(:second))  // Create a new random number generator
let random_number = Random.int(1, 100, rng)  // Generate a random integer between 1 and 100
```

## गहरा डाइव (Deep Dive)

रैंडम नंबर उत्पन्न करने के लिए प्राचीन प्रविधियों में घडियाल, गिणती टाइमर, मिलीसेकंड डेटाबेस इत्यादि शामिल थे। आधुनिक पढ़ाव में प्रोग्रामर्स हार्डवेयर और सॉफ्टवेयर के माध्यम से प्रमाणीकरण, कूटलेखन, क्रिप्टोग्राफी एडियो और वीडियो स्ट्रीम में यादृच्छिकता प्रविष्ट करसकते हैं।

वैकल्पिक रूप से, Erlang/OTP का :rand मॉडल भी गलीम में उपलब्ध है जो Exsplus आदि प्रामाणिक क्रिप्टोग्राफिक अल्गोरिदम का समर्थन करता है।

## यह भी देखें (See Also)

1. Gleam डॉक्स: रैंडम संख्या - (https://gleam.run/book/tour/random-numbers.html)
2. Erlang :rand मॉडल डॉक्स - (http://erlang.org/doc/man/rand.html)
3. प्रासंजनिक जित्तेर परिप्रेक्ष्य:  (https://en.wikipedia.org/wiki/Hardware_random_number_generator)
4. प्रासंजनिक जित्तेर GitHub - (https://github.com/smuellerDD/jitterentropy-library)