---
title:                "एक वेब पेज डाउनलोड करना"
html_title:           "Kotlin: एक वेब पेज डाउनलोड करना"
simple_title:         "एक वेब पेज डाउनलोड करना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
वेब पेज डाउनलोड करना, वेबसाइट की एक कॉपी को लोकल सिस्टम पर सहेजने की प्रक्रिया है। प्रोग्रामर इसे डाटा संग्रहित करने, विश्लेषण करने या बाद में इसे ऑफ़लाइन उपयोग करने के लिए करते हैं। 

## कैसे करें:
जैसा कि आप देख सकते हैं, इसे Gleam कोड के माध्यम से आसानी से प्रदर्शित किया जा सकता है:

```Gleam
import gleam/httpc
import gleam/uri

fn download_page(url: String) {
  let url = uri.parse(url)
  let response = httpc.get(url)

  case response {
    Ok(body) -> io.println(body)
    Error(_) -> io.println("Unable to download the page")
  }
}
```
और उत्तर होता है:
```
{response body here if successful}
```
या
```
Unable to download the page
```

## गहरा जरीप 
वेब पेजों को डाउनलोड करने की आवश्यकता हमेशा से ही रही है, चाहे वह मानव उपयोगकर्ता निगरानी के लिए हो या बॉट्स की स्क्रैपिंग। प्राचीन 'wget' और 'curl' जैसे उपकरण अब भी उपयोग होते हैं, लेकिन मोडर्न प्रोग्रामिंग भाषाओं ने इन प्रक्रियाओं को काफी सरल बना दिया है। 

विकल्पाः अन्य भाषाओं में भी इसे करने का उपाय मौजूद है, जैसे कि Python के requests लाइब्रेरी या Javascript का axios लाइब्रेरी। 

आप उसे तब भी संशोधित कर सकते हैं, जैसे कि कस्टम हेडरों के साथ अनुरोध भेजें, या विभिन्न HTTP स्थितियों का उपयोग करें। 

## अन्य स्रोतों के लिए लिंक 
1. वेब स्क्रेपिंग के लिए Python रिक्वेस्ट्स: https://docs.python-requests.org/en/master/
2. Javascript के लिए axios: https://github.com/axios/axios
3. Gleam प्रलेखन: https://gleam.run/docs/