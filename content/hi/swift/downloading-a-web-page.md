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

## Kya Aur Kyon?
Web page download karna hume internet par maujud itni sari jankari ka ek saath access deta hai. Isme hum kisi bhi website ki code, images, videos aur text ko apne device par save kar sakte hai. Programming mein web page download karna ek common task hai, jisse hum apne apps mein internet se data retrieve kar sakte hai.

## Kaise Karein:
Iss article mein hum Swift programming language mein kaise web page download karte hai uske baare mein jaanenge. Neeche diye gaye code examples aur sample output use karke aapko is process ko samajhne mein aasani hogi.

```Swift
// 1. URL object create karein
let url = URL(string: "https://www.example.com")
 
// 2. URLSession create karein
let session = URLSession.shared
 
// 3. URLSessionDataTask create karein
let dataTask = session.dataTask(with: url!, completionHandler: {
    data, response, error in
     
    // 4. Response check karein
    guard let httpResponse = response as? HTTPURLResponse, httpResponse.statusCode == 200, error == nil else {
        print("Error downloading data.")
        return
    }
     
    // 5. Data decode karein
    guard let receivedData = data else {
        print("Error loading data.")
        return
    }
     
    // 6. Data output karein
    if let dataString = String(data: receivedData, encoding: .utf8) {
        print(dataString)
    }
})
 
// 7. Task ko resume karein
dataTask.resume()
```

**Output:** Iss code ka output aapke console mein downloaded web page ki HTML code print karega.

## Deep Dive:
Web page download karna programming mein common task hai kyonki isse hum apne apps mein internet se data retrieve kar sakte hai. Pehle log raw socket programming ka use karte the lekin ab URL loading system provide karta hai, jo easily data retrieve karne mein help karta hai. Swift ke alawa, aap URL loading system ka use Objective-C, Java, Python aur PHP jaise aur bhi languages mein kar sakte hai.

## See Also:
1. [The Swift Programming Language by Apple](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html)
2. [Apple's URL Loading System Guide](https://developer.apple.com/documentation/foundation/url_loading_system)