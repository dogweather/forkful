---
title:                "डायरेक्टरी का अस्तित्व जाँचना"
date:                  2024-01-20T14:58:54.389885-07:00
html_title:           "Elm: डायरेक्टरी का अस्तित्व जाँचना"
simple_title:         "डायरेक्टरी का अस्तित्व जाँचना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

डायरेक्टरी का अस्तित्व जांचना मतलब है पता लगाना कि एक फोल्डर मौजूद है या नहीं। प्रोग्रामर्स इसलिए जांचते हैं ताकि एरर से बच सकें और फाइल्स को सही जगह पर सेव कर सकें।

## How to: (कैसे करें)

Ruby में यह जांचना कि एक डायरेक्टरी मौजूद है या नहीं, बहुत सीधा है:

```Ruby
require 'fileutils'

# यह जांच करता है कि डायरेक्टरी मौजूद है कि नहीं
if Dir.exists?("/path/to/directory")
  puts "डायरेक्टरी मौजूद है!"
else
  puts "डायरेक्टरी मौजूद नहीं है।"
end
```

कोड चलाने पर सैंपल आउटपुट यह हो सकता है:

```
डायरेक्टरी मौजूद है!
```
या

```
डायरेक्टरी मौजूद नहीं है।
```

## Deep Dive (गहराई में जानकारी)

पहले, Ruby में डायरेक्टरी चेक करने के लिए 'FileTest' मॉड्यूल प्रयोग किया जाता था, जिसमें `exist?` मेथड होता था। हालांकि, रूबी में बदलाव आए हैं और `Dir.exists?` प्रयोग करना अधिक सामान्य हो गया है। इसके अलावा `File.directory?` मेथड भी होता है जिससे जांचा जा सकता है कि एक पथ डायरेक्टरी को इंगित करता है या नहीं।

```Ruby
if File.directory?("/path/to/directory")
  # कोड जब डायरेक्टरी मौजूद हो
end
```

प्रोग्रामर्स को ऐसा करना चाहिए क्योंकि यह एप्लिकेशन को ऐसे संभावित एरर्स से बचाता है जो तब हो सकते हैं जब एक डायरेक्टरी में फाइल लिखने / पढ़ने की कोशिश की जाती है और वह डायरेक्टरी मौजूद नहीं होती।

## See Also (अन्य जानकारी)

- Ruby डॉक्यूमेंटेशन `Dir.exists?`: https://ruby-doc.org/core-2.7.0/Dir.html#method-c-exists-3F
- Stack Overflow पर पूछे गए सम्बंधित प्रश्न: https://stackoverflow.com/questions/tagged/ruby+directory
- Ruby में फाइल हैंडलिंग और डायरेक्टरी मैनेजमेंट पर गाइड: https://www.rubyguides.com/2015/12/working-with-files-ruby/