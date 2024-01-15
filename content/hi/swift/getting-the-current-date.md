---
title:                "वर्तमान तिथि प्राप्त करना"
html_title:           "Swift: वर्तमान तिथि प्राप्त करना"
simple_title:         "वर्तमान तिथि प्राप्त करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

# आखिर तारीख कैसे प्राप्त की जाती है?

## क्यों

तारीख को प्राप्त करने का सबसे साधारण और उपयोगी तरीका है क्योंकि यह विभिन्न प्रोग्रामों में उपयोगी हो सकती है। यह चरण किसी भी सोच वाले व्यक्ति की स्थानांतरित 灙रकेर ईनपुट से हो सकती है और सोपान से भी लागू हो सकती है।

## कैसे

```Swift
let date = Date()
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "MM-dd-yyyy"
let formattedDate = dateFormatter.string(from: date)
print(formattedDate) // 06-09-2021
```

यहां हमने Date और DateFormatter की सहायता से आज की तारीख प्राप्त की है। हमने Date का एक ऑब्जेक्ट बनाया है और उसे DateFormatter के द्वारा फॉर्मेट किया है। आप विभिन्न फॉर्मेट जैसे "MM-dd-yy", "yyyy-MM-dd" आदि का उपयोग कर सकते हैं। फॉर्मेटेड तारीख को string() के द्वारा प्रिंट किया जा सकता है।

## गहराई में जाएं

आप Swift में दिए गए तारीख का एक ऑब्जेक्ट बनाकर उसे प्राप्त कर सकते हैं। आप अपनी आवश्यकताओं के अनुसार Date को format कर सकते हैं और विभिन्न महीनों, सालों, गणनाओं और इसके साथ रात और दिन को manipulate कर सकते हैं।

# देखें भी

- [Swift Date and Time Functions](https://www.tutorialspoint.com/swift/swift_date_time.htm)
- [Apple's Documentation on Date](https://developer.apple.com/documentation/foundation/date)