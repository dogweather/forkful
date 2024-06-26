---
date: 2024-01-26 04:18:02.316306-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Ruby \u0915\u093E\
  \ REPL IRB (Interactive Ruby) \u0915\u0939\u0932\u093E\u0924\u093E \u0939\u0948\u0964\
  \ \u0905\u092A\u0928\u0947 \u091F\u0930\u094D\u092E\u093F\u0928\u0932 \u0938\u0947\
  \ \u0938\u0940\u0927\u0947 Ruby \u0915\u094B \u0906\u091C\u092E\u093E\u0915\u0930\
  \ \u0926\u0947\u0916\u0947\u0902."
lastmod: '2024-03-13T22:44:53.226856-06:00'
model: gpt-4-0125-preview
summary: "Ruby \u0915\u093E REPL IRB (Interactive Ruby) \u0915\u0939\u0932\u093E\u0924\
  \u093E \u0939\u0948\u0964 \u0905\u092A\u0928\u0947 \u091F\u0930\u094D\u092E\u093F\
  \u0928\u0932 \u0938\u0947 \u0938\u0940\u0927\u0947 Ruby \u0915\u094B \u0906\u091C\
  \u092E\u093E\u0915\u0930 \u0926\u0947\u0916\u0947\u0902."
title: "\u0907\u0902\u091F\u0930\u0948\u0915\u094D\u091F\u093F\u0935 \u0936\u0947\u0932\
  \ (REPL) \u0915\u093E \u0909\u092A\u092F\u094B\u0917"
weight: 34
---

## कैसे करें:
Ruby का REPL IRB (Interactive Ruby) कहलाता है। अपने टर्मिनल से सीधे Ruby को आजमाकर देखें:

```Ruby
irb
2.7.0 :001 > puts "नमस्ते, Ruby वर्ल्ड!"
नमस्ते, Ruby वर्ल्ड!
 => nil
2.7.0 :002 > 5.times { print "Ruby! " }
Ruby! Ruby! Ruby! Ruby! Ruby!  => 5
```

## गहन जांच
Ruby 1.8 में पेश किया गया, IRB Rubyists के लिए एक मुख्य आधार है। यह Lisp और Python के इंटरेक्टिव शेल्स से प्रेरित है, प्रयोग और तत्काल प्रतिक्रिया को मिलाकर। जैसे कि Pry जैसे वैकल्पिक विकल्प सिंटैक्स हाईलाइटिंग और एक अधिक शक्तिशाली डिबगिंग वातावरण जैसी अधिक सुविधाएं प्रदान करते हैं। IRB स्वयं सरल है लेकिन 'irbtools' जैसे gems के साथ सम्पूर्ण कार्यक्षमता का विस्तार किया जा सकता है। IRB रीड-एवल-प्रिंट लूप को कैसे संभालता है, इसके द्वारा प्रत्येक इनपुट लाइन को पढ़कर, इसे Ruby कोड के रूप में मूल्यांकन करके, और फिर परिणाम को प्रिंट करके, इस प्रक्रिया को निकास तक दोहराता है।

## देखें भी
- [Ruby का IRB](https://ruby-doc.org/stdlib-2.7.0/libdoc/irb/rdoc/IRB.html)
- [The irbtools gem](https://github.com/janlelis/irbtools)
