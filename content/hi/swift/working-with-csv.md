---
title:                "CSV के साथ काम करना"
html_title:           "Swift: CSV के साथ काम करना"
simple_title:         "CSV के साथ काम करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## क्यों
CSV फाइलों के साथ काम करने में कौन समय और मेहनत बचाना चाहता है, उनके लिए यह लाभदायक हो सकता है। 

## कैसे करें
```Swift
// CSV फाइल खोलना
if let csvURL = Bundle.main.url(forResource: "data", withExtension: "csv") {
    do {
        let csvString = try String(contentsOf: csvURL, encoding: .utf8)
        // स्ट्रिंग विभाजित करना
        let rows = csvString.components(separatedBy: .newlines)
        for row in rows {
            // वर्तमान पंक्ति को टोकन बनाना
            let tokens = row.components(separatedBy: ",")
            for token in tokens {
                print(token)
            }
        }
    } catch {
        print("CSV फाइल खोलने में त्रुटि: \(error)")
    }
}
```

उपरोक्त कोड में, हम CSV फाइल को खोलने के लिए Bundle वर्ग का उपयोग किया है, जिसे main Bundle से गुजरने के लिए कॉल किया गया है। उसके बाद, हम शिकंजे और पंक्तियों को विभाजित करने के लिए String वर्ग और इस्तेमाल किए गए components से उपयोग किया है। हमें वर्तमान पंक्तियों से टोकन बनाने की जरूरत होती है, जो फिर से components का उपयोग करके हो सकती है। आप टोकनों को अपनी आवश्यकतानुसार उपयोग कर सकते हैं, जैसे कि इन टोकनों को प्रिंट करने के लिए हमने किया है। 

## गहराई से जानें
CSV या "Comma Separated Values" एक विशेष गोपनीयता धारपात का स्थान है जिसे यह उद्देश्य किया गया है कि प्रोग्रामिंग और इंटरनेट के साथ काम करने वालों को डेटा को संगठित करने में आसानी हो। ये फाइलें अक्सर स्प्रेडशीट और डेटा बेस से डेटा को लोड करने के लिए किये हुए जाते हैं। Swift इस CSV परिचालन को अत्यंत सरल और आसान बनाता है, जो प्रोग्राम