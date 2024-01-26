---
title:                "नई परियोजना शुरू करना"
date:                  2024-01-20T18:03:34.300623-07:00
model:                 gpt-4-1106-preview
simple_title:         "नई परियोजना शुरू करना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

नई प्रोजेक्ट शुरू करना मतलब एक खाली कैनवास पर अपने विचारों को कोड के रूप में उकेरना है। प्रोग्रामर इसलिए नए प्रोजेक्ट शुरू करते हैं क्योंकि वो नए आइडियाज को अमल में लाना चाहते हैं, या किसी समस्या का हल निकालना चाहते हैं।

## How to: (कैसे करें:)

Gleam में नया प्रोजेक्ट बनाने के लिए, `gleam new` कमांड का इस्तेमाल करें। उदाहरण के लिए:

```gleam
gleam new my_cool_project
```

इस कमांड से आपको एक नया फोल्डर मिलेगा जिसमें सभी जरूरी फाइलें होंगी जैसे कि `rebar.config` और एक `src` डायरेक्टरी जिसमें आपका मेन `my_cool_project.gleam` फाइल होगा।

## Deep Dive (गहराई में जानकारी):

Gleam, एक Type-Safe, फंक्शनल प्रोग्रामिंग भाषा है, जिसे Rust inspired syntax के साथ डिजाइन किया गया है। यह Erlang इकोसिस्टम पर आधारित है और BEAM virtual machine पर चलता है। ग्लीम की ताकत इसमें स्ट्रांग टाइपिंग और फॉल्ट-टोलरेंस में है।

वैकल्पिक तौर पर, लोग Elixir या Erlang जैसी अन्य भाषाओं का भी चुनाव कर सकते हैं। जब आप Gleam का इस्तेमाल करते हैं, आपको एक शक्तिशाली टाइप सिस्टम मिलता है जो आपके एरर्स को compile-time पर ही पकड़ लेता है।

## See Also (अधिक जानकारी के लिए):

- Gleam की आधिकारिक वेबसाइट: [https://gleam.run/](https://gleam.run/)
- ग्लीम के साथ काम करने के लिए गाइड: [https://gleam.run/book/](https://gleam.run/book/)
- Gleam GitHub Repository: [https://github.com/gleam-lang/gleam](https://github.com/gleam-lang/gleam)
