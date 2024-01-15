---
title:                "स्टैंडर्ड त्रुटि पर लेखन"
html_title:           "Swift: स्टैंडर्ड त्रुटि पर लेखन"
simple_title:         "स्टैंडर्ड त्रुटि पर लेखन"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## क्यों 

अभी हाल ही में, Swift में लिखने वाले कई डेवलपर हाई-लेवल logging एपीआई का उपयोग करते हैं जो उन्हें debugging के लिए मदद करता है। इसमें लिखने के साथ, standard error को लिखने का कोई विस्तारित अवसर नहीं है जो कि एक कामतरता वृध्दि वाली तकनीक है इससे आप अपने प्रोग्राम को आसानी से debug कर सकते हैं।

## कैसे करें 

```Swift
// त्रुटि संदेश को standard error में लिखने का उदाहरण
print("Error: Unable to perform operation", to: &errorStream)

// समान त्रुटि संदेश जो कि standard output में लिखा हुआ होता है
print("Error: Unable to perform operation")

// त्रुटि संदेशों को अन्य errors के साथ एकीकृत करने का उदाहरण
let error = NSError(domain: "com.example.error", code: 500, userInfo: [NSLocalizedDescriptionKey: "Unable to perform operation"])
print(error, to: &errorStream)
```

उपरोक्त उदाहरणों को देखें, हमने `print()` फंक्शन का उपयोग किया है जो कि standard error में लिखने के लिए एक अलग विकल्प प्रदान करता है। इसके अलावा, हमने `NSError` की मदद से अन्य errors को एकीकृत किया है जो कि standard error में लिखा जाएगा।

## गहराई में जाएँ 

Standard error के उपयोग को आपके प्रोग्राम को debug करने में बहुत मदद मिल सकती है। इसके अलावा, अगर आपका प्रोग्राम अन्य प्रोग्रामों को कॉल करता है तो आप standard error में errors को लिख सकते हैं जो कि आपको समस्या का पता लगाने में मदद करेंगे।

## देखें भी 

- [Apple Developer Documentation: Using Standard Error and Standard Output](https://developer.apple.com/documentation/swift/formatoutput/using_standard_error_and_standard_output)