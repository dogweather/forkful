---
title:                "दो तारीखों की तुलना"
html_title:           "Elixir: दो तारीखों की तुलना"
simple_title:         "दो तारीखों की तुलना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

तारीखों की तुलना (Comparing dates) से हमारा तात्पर्य दो तारीखों को आपस में तुलना करने से है। कोडर इसे तब करते हैं जब उन्हें निश्चित करना होता है कि कौन सी तारीख पहली हुई और कौन सी बाद में आई, या क्या वे दोनों समान हैं।

## कैसे:

इसे Swift में करने का तरीका निम्नानुसार है:

```Swift
import Foundation
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd.MM.yyyy"
let date1 = dateFormatter.date(from: "10.05.2021")
let date2 = dateFormatter.date(from: "15.05.2021")

if let firstDate = date1, let secondDate = date2 {
    if firstDate < secondDate {
        print("Date1 is before Date2")
    } else if firstDate > secondDate {
        print("Date1 is after Date2")
    } else {
        print("The two dates are the same")
    }
}
```

आउटपुट ऐसा होगा:

```Swift
"Date1 is before Date2"
```

## गहराई में:

1. ऐतिहासिक संदर्भ: हम सभी जानते हैं कि समय और तारीख की गणना करना एक पुराना विचार है। हालांकि, इसे कम्प्यूटर कोडिंग में लागू करने का तरीका नए ज़माने के साथ बदल गया है।
    
2. विकल्प: NSCalendar और NSDate का उपयोग करके भी इसे किया जा सकता है, लेकिन हमने यहाँ DateFormatter का इस्तेमाल किया है क्योंकि यह कुछ अधिक प्रतिसाद-प्रवेशी होता है।
    
3. कार्यान्वयन विवरण: हम DateFormatter का इस्तेमाल कर रहे हैं, जो Foundation फ्रेमवर्क का हिस्सा है और तिथि के कच्चे मानों को काम करने के लिए मनुष्य पठनीय रूप में परिवर्तित करने में सहायता करता है।

## देखने के लिये:

1. [Apple's Documentation on Dates and Times](https://developer.apple.com/documentation/foundation/dates_and_times)
2. [NSDate Comparison in Swift](https://stackoverflow.com/questions/27182023/getting-the-difference-between-two-nsdates-in-months)
3. [Apple's DateFormatter Documentation](https://developer.apple.com/documentation/foundation/dateformatter)

---------