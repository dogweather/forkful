---
title:                "मानक त्रुटि में लिखना"
date:                  2024-01-19
html_title:           "Arduino: मानक त्रुटि में लिखना"
simple_title:         "मानक त्रुटि में लिखना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
स्टैंडर्ड एरर पर लिखना यानी आपके प्रोग्राम की ग़लतियों को एक विशेष जगह पर दिखाना. यह प्रोग्रामर्स इसलिए करते हैं क्योंकि यह आउटपुट और एरर मैसेजेज को अलग रखता है, जिससे डिबगिंग आसान हो जाती है.

## How to: (कैसे करें:)
```Ruby
# सिंपल मेसेज को स्टैंडर्ड एरर में लिखना
$stderr.puts 'यह एक त्रुटि संदेश है'

# स्टैंडर्ड एरर में डायरेक्टली लिखना
$stderr.print 'और एक एरर!'

# अगर आपको अपने प्रोग्राम में STDERR का इस्तेमाल करना है
STDERR.puts 'STDERR का इस्तेमाल करते हुए एरर मेसेज'
```
सैंपल आउटपुट:
```
यह एक त्रुटि संदेश है
और एक एरर!
STDERR का इस्तेमाल करते हुए एरर मेसेज
```

## Deep Dive (गहराई में जानकारी):
स्टैंडर्ड एरर (stderr) एक स्ट्रीम है जो UNIX में शुरू हुई थी. यह अक्सर स्टैंडर्ड आउटपुट (stdout) से अलग होती है. Ruby में `$stderr` और `STDERR` इसकी पहचान हैं. वैकल्पिक रूप से, प्रोग्रामर `IO#write` और `IO#puts` जैसे मेथड का उपयोग कर सकते हैं या लॉगिंग लाइब्रेरीज का उपयोग कर सकते हैं.

## See Also (संबंधित स्रोत):
- Ruby Docs on I/O: [https://ruby-doc.org/core-3.1.0/IO.html](https://ruby-doc.org/core-3.1.0/IO.html)
- Wikipedia page on Standard streams: [https://en.wikipedia.org/wiki/Standard_streams](https://en.wikipedia.org/wiki/Standard_streams)
