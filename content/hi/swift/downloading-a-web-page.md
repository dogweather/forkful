---
title:                "एक वेब पेज डाउनलोड करना"
html_title:           "Swift: एक वेब पेज डाउनलोड करना"
simple_title:         "एक वेब पेज डाउनलोड करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्यों
क्या आप भी कभी वेब पेज डाउनलोड की आवश्यकता महसूस करते हैं? वेबसाइटों का संदर्भ (reference) महत्वपूर्ण हैं और इन्हें अपने कंप्यूटर पर सुरक्षित रूप से डाउनलोड और संशोधित करना आपको दृश्यमानता (visibility) और सुविधा (convenience) देता है।

## कैसे करें
```Swift
let url = URL(string: "https://www.examplewebsite.com/") // वेब पेज का URL
let request = URLRequest(url: url!) // वेब पेज का अनुरोध बनाएं
let task = URLSession.shared.dataTask(with: request) { // डेटा को डाउनलोड करें
    (data, response, error) in
    if let error = error { // यदि कोई त्रुटि है तो उसकी जांच करें
        print("त्रुटि: \(error)")
    } else {
        if let data = data { // यदि डेटा मौजूद है तो उसे प्रिंट करें
            print(data)
        }
    }
}
task.resume() // अनुरोध को शुरू करें
```

यहां हमने `URLSession` का इस्तेमाल करके डेटा को डाउनलोड करने का तरीका दिखाया है। आप चाहे तो `URLSession` के बजाय `URLSessionConfiguration` का भी प्रयोग कर सकते हैं। अपनी सुविधा के अनुसार कोडिंग करें।

## गहराई में जाएं
वेब पेज को डाउनलोड करने के लिए सीधी आदेश (direct command) `curl` भी हो सकता है। लेकिन `curl` का उपयोग `URLSession` से करने के मुकाबले कठिन हो सकता है। इसलिए, आपको `URLSession` और `URLSessionConfiguration` का इस्तेमाल सीखना चाहिए। पढ़े [Yalini Ravi की रचनाएं](https://yaliniravi.medium.com/downloading-files-using-swifts-urlsession-and-urlsessionconfiguration-427132956f0d) जो आपको डेटा को डाउनलोड करने में मदद करेंगी।

## देखें और होगा अगला (See Also)
- [Apple के रिकमंदेशं (guidelines) को पढ़ें](https://developer.apple.com/documentation/foundation/urlsession) जो `URLSession` और `URLSessionConfiguration` के ब