---
title:                "वर्तमान तारीख प्राप्त करना"
html_title:           "C#: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

समय और तारीख को प्राप्त करना यहां के लिए महत्वपूर्ण हैं क्योंकि कई बार हमें डाटा को टाइम स्ट्रिंग्स के रूप में कनवर्ट करने की आवश्यकता होती है, HIV और संबंधित ऑपरेशन्स की गणना करनी होती है। विभाजन के साधारण उद्देश्यों में समय और तारीख की जानकारी शामिल होती है। 

## कैसे:

वर्तमान दिनांक को प्राप्त करने के लिए निम्न कोड स्निपेट का उपयोग करता है:

```Ruby
require 'date'

aab = Date.today
puts aab
```

यह कोड संगठनात्मक चर्चा को छापेगा (जैसे `2025-10-15`).

## गहरी गोता

1. ऐतिहासिक संदर्भ: रूबी के पहले संस्करण में Date class की समय और तारीख की गणना का समर्थन नहीं था। 'Date' लाइब्रेरी को बाद में जोड़ा गया जिसने इस क्षेत्र में महत्वपूर्ण सुधार किए।
2. विकल्प: अन्य तारीख और समय के लाइब्रेरी भी उपलब्ध हैं, जैसे कि 'time' और 'datetime'। आपके उपयोग के लिए सबसे अच्छा क्या है, यह आपकी उपयोगिता की आवश्यकताओं पर निर्भर करता है।
3. कार्यान्वितान विवरण: Date.today actually वास्तविक तारीख को उत्पन्न करने के लिए सिस्टम क्लाक का उपयोग करता है। 

## अन्य जानकारी के लिए:

1. [Ruby Doc Date class](https://ruby-doc.org/stdlib-3.0.2/libdoc/date/rdoc/Date.html)
2. [Ruby Doc DateTime class](https://ruby-doc.org/stdlib-3.0.2/libdoc/date/rdoc/DateTime.html)
3. [Ruby Doc Time class](https://ruby-doc.org/core-3.0.2/Time.html)