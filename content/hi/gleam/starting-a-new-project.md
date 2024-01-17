---
title:                "नया प्रोजेक्ट शुरू करना"
html_title:           "Gleam: नया प्रोजेक्ट शुरू करना"
simple_title:         "नया प्रोजेक्ट शुरू करना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why?

एक नया परियोजना शुरू करना क्या है और क्यों कोडर्स इसे करते हैं? परियोजना शुरू करना बस एक नया सॉफ्टवेयर या एप्लिकेशन बनाना है और कोडर्स इसे अपने कैरियर का विकास और बढ़ाने के लिए करते हैं।

## How to:

```Gleam
module Example

import gleam/json

pub fn main() {
  let my_name = "John"
  let message = greeting(my_name)
  json.encode(message)
}

fn greeting(name) {
  "Hello, " <> name
}
```

इस सादे और सरल कोड में, हमने एक इनपुट के नाम को लेकर एक ग्रीटिंग बनाया है और उसे JSON में encode किया है। जैसा कि आप देख सकते हैं, Gleam में कोडिंग बहुत ही आसान है - सिर्फ वेरिएबल declare करें, फ़ंक्शन बनाएं और उन्हें स्ट्रिंग के साथ जोड़ें।

```Gleam
let fruits = [
  "apple",
  "banana",
  "orange"
]

pub fn main() {
  print_last_item(fruits)
}

fn print_last_item(list) {
  let last_item = list. last()
  IO.print("Last item is " <> last_item)
}
```

यहां, हमने एक लिस्ट के पिछले आइटम को प्रिंट किया है। गलती से आपने नोटिस किया होगा कि हमने लिस्ट Declare नहीं किया है, और हमने इसे काम कार्यो में ही सिर्फ इन्हें सूची के ज्यादातर फ़ंक्शन में इस्तेमाल किया है। इसलिए, आपने देखा कि Gleam ने variable और constants को कितना सरल और चुनौतीपूर्ण बना दिया है।

## Deep Dive:

- Historical Context: Gleam, जिसे मूल तौर पर एक फ़ंक्शनल प्रोलोग दिया गया था, बहुत ही ग्राईम एंड ग्रफीयिंक अनुकूल है। प्रोलोगके साथ सोपवस्तु से जुड़े हुए कोम्पियूटेशनल फ़ंक्शन को जब डिवेलप ने जब दिया तो, यह कहा गया कि विक्टोर तोर्रेंट और डमिनिक कम्पेनीस ने उसको इस के पहले ही कई बार स्टडी किया था! ।
- Alternatives: अन्य प्रोग्रामिंग भाषाएं जैसे कि जावा, सी++, और पाइथन वर्तमान में सबसे लोकप्रिय हैं। हालांकि, ग्राह्यिकी और पार्टिलिटि की दृश्यःअन रों कंपाइलेशनों के अनुलज़ डेभलपमन, जैसे कि, सामन पोनामियम् सॉब (Superbar), हॉर्नेट (Hornet), और गियाछ शब्दकोष (Guyachika शब्दकोष)।
- Implementation details: Gleam जेड कॉम्प्राब्ले (nifft-bets) और इजीनन्स, जो प्रोगाडव्दुओ सतमि का प्रोडूजस रहा है, प्रोग्राम की जड़से और वदने डारवाह (वञानांकःव अनुरीव, जिसे कि अन्य लोग चेले सोन के हैं इस ल