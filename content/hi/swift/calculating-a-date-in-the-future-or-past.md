---
title:                "भविष्य या अतीत में एक तारीख की गणना"
html_title:           "Swift: भविष्य या अतीत में एक तारीख की गणना"
simple_title:         "भविष्य या अतीत में एक तारीख की गणना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

विभावीय या भूतकालीन तारीख की गणना का मतलब होता है वर्तमान तारीख से अधिक या कम दिन की गणना करना। प्रोग्रामर्स इसे तारीख को मिलाने, समय के अंतर की गणना और छुट्टियों और घटनाओं के लिए अनुसूचियाँ बनाने के लिए करते हैं।

## कैसे करें:

यहां Swift में विभावीय तारीख की गणना करने का एक उदाहरण है:

```Swift
import Foundation

let date = Date()
let calendar = Calendar.current

let futureDate = calendar.date(byAdding: .day, value: 5, to: date)

print(date)
print(futureDate ?? Date())
```
यह कोड वर्तमान तारीख के 5 दिन बाद की तारीख की गणना करता है।

## गहरी जांच:

डेट और टाइम की गणना करने के तरीके के विकास में कई चरण रहे हैं, बाद में Swift ने इसे और अधिक सुविधाजनक बना दिया है। `Calendar.current` और `.date(byAdding:)` मेथड्स का उपयोग कर के आप विभावीय या भूतकालीन तारीख की गणना कर सकते हैं। वैकल्पिक तरीके में, आप NSDateComponents और NSCalendar का उपयोग कर सकते हैं, लेकिन यह Swift के नवीनतर संस्करणों में ओबसोलिट हो गई है।

## यह भी देखें:

- Apple का आधिकारिक Swift प्रलेखन (https://developer.apple.com/documentation/swift)
- Date और Time का प्रबंधन Swift में (https://www.hackingwithswift.com/read/12/3/how-to-manipulate-dates-and-times-using-datecomponents-and-date)