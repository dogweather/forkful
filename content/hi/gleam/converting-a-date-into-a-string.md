---
title:                "एक तारीख को स्ट्रिंग में बदलना।"
html_title:           "Gleam: एक तारीख को स्ट्रिंग में बदलना।"
simple_title:         "एक तारीख को स्ट्रिंग में बदलना।"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
डेटा को स्ट्रिंग में बदलना यानि डेटा का एक स्ट्रिंग वर्जन बनाना हमारे कोड में संभावित तिथियों को शामिल करने का एक तरीका है। प्रोग्रामर्स अक्सर इसे डेटा प्रसंस्करण और दिखाने के नियमित काम के रूप में करते हैं। 

## कैसे करें:
```Gleam
let date = 2021-07-14
let str_date = to_string(date, "%d %b, %Y")
IO.println(str_date)
```

आप इस तरह से स्ट्रिंग में डेटा को बदल सकते हैं और अपनी तिथि को एक वर्णमाला में प्रदर्शित कर सकते हैं। यहां हमने दिनांक "14 जुलाई, 2021" में गाइड के मुताबिक प्रदर्शित किया है। 

## गहराई में जाइए:
जब हम अपने कोड में डेटा को स्ट्रिंग में बदलते हैं तो हम इसे बीच में "%%" उपयोग करके फॉरमेट कर सकते हैं। इसके अलावा, आप दिनांक को अलग अलग फॉर्मैट में प्रदर्शित कर सकते हैं, जैसे "14/07/2021" या "July 14, 2021"। इसके अलावा, कई अन्य भाषाओं में भी समर्थन है। जैसे कि अगर आप अंग्रेजी के स्थान पर हिंदी में अपनी तिथि को प्रदर्शित करना चाहते हैं, तो आप "dd mmmm yyyy" फॉर्मेट का इस्तेमाल कर सकते हैं। 

## और भी देखें:
- Date to String Conversion in Gleam: https://gleam.run/articles/date-string-conversion.html
- String Formatting in Gleam: https://gleam.run/articles/string-formatting.html
- Gleam docs on string: https://gleam.run/docs/std/string.html