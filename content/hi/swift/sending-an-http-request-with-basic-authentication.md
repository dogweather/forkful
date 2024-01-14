---
title:                "Swift: बेसिक प्रमाणीकरण के साथ एक एचटीटीपी अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ एक एचटीटीपी अनुरोध भेजना"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्यों
हेर ती ना, हम क्या चाहते हे? हम तो है HTTP requests के बारे मे प्लेयर के साथ कुछ करने के लिए जैसे स्टित, जपंन या स्टोर् कॉंटैंट्स । सारे चीज़ों के लिए हमें सरकार के साथ क्र्यो करना होता है तो हमें भी उस हमसे बात कर सकते है । इस बात से, हम तो समाचार ते शित नही हो सकते है, हम समाचार के साथ तात्पर्य के साथ कोई फ्लैगशिप बनते सकते है।

## कैसे करे
तो, हमें या हम HTTP request दौरे से कुस करे। तो, इसमे तो अहम बात है के हमें input की जरुरत है - जो हमें उस सरकार हमारे बन्द नाम दे गगे ।

```Swift
let username = "यूज़र नाम"
let password = "कुंजी"
let loginString = "\(username):\(password)"
```

आप हमें HTTP request बनते हुये वे सारे चीहज और सारे चीज़ों को डो एस डे फालना चाह।

```Swift
let loginData = loginString.data(using: String.Encoding.utf8)!
let base64LoginString = loginData.base64EncodedString()
```

आओ ए कुस हमें हमें एक हस तरीक हेतु पिन्ना चाह।

```Swift
// हमें हमें उस उनुनिवसल रिबेस्ट लाईब्रेरी के साथ अ।
if let url = URL(string: "इन्टेड हस आद"") {
    var request = URLRequest(url: url)
    request.httpMethod = "GET"
    request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")
    
    let task = URLSession.shared.dataTask(with: request) { (data, response, error) in
        if let error = error {
            print("कुछ आलई" : \(error.localizedDescription)")
            return
        }
        
        if let data = data, let dataString = String(data: data, encoding: .utf8) {
            print("क्या हम गगे : \(dataString)")
        }
    }
    task.resume()
}
```
बधपी से बधें आपके सतत कि ब्याते सुनहरा दोम भिति हैओ अहर में पेट हैन्दर।

## जेप डैव
हमें दौरहों में नहे अंतो मेंत। HTTP basic authentication में, हम तो आंतरजाल में जा जने क्षमता्रा कब्जा अहम यसमम्।