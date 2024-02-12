---
title:                "इंटरैक्टिव शेल (REPL) का उपयोग"
aliases:
- hi/ruby/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:18:02.316306-07:00
model:                 gpt-4-0125-preview
simple_title:         "इंटरैक्टिव शेल (REPL) का उपयोग"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
एक इंटरेक्टिव शेल, या REPL (Read-Eval-Print Loop), आपको वास्तविक समय में कोड का परीक्षण करने की अनुमति देता है। प्रोग्रामर इसका उपयोग प्रयोग, डिबगिंग, और पूर्ण रूप से स्क्रिप्ट बनाए बिना Ruby की बारीकियों को सीखने के लिए करते हैं।

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
