---
title:                "वर्तमान तारीख प्राप्त करना"
aliases:
- /hi/ruby/getting-the-current-date.md
date:                  2024-02-03T19:11:01.415114-07:00
model:                 gpt-4-0125-preview
simple_title:         "वर्तमान तारीख प्राप्त करना"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
वर्तमान तारीख प्राप्त करना लगभग किसी भी प्रोग्रामिंग प्रयास में एक आवश्यक कार्य है, एप्लिकेशन में गतिविधियों की लॉगिंग से लेकर तारीख मोहरों के साथ रिपोर्ट्स जेनेरेट करने तक। Ruby में, यह मानक पुस्तकालय का उपयोग करके आसानी से हासिल किया जा सकता है, जो तारीखों के साथ संचालन को सरल बनाता है।

## कैसे करें:
Ruby का मानक पुस्तकालय तारीखें और समय संभालने के लिए `Date` और `Time` क्लासेज को शामिल करता है। यहाँ है कैसे वर्तमान तारीख प्राप्त करें:

```ruby
require 'date'

current_date = Date.today
puts current_date
```

नमूना आउटपुट: 
```
2023-04-12
```

तारीख के साथ समय शामिल करने के लिए, Ruby का `Time` क्लास अधिक उपयुक्त है:

```ruby
current_time = Time.now
puts current_time
```

नमूना आउटपुट: 
```
2023-04-12 14:33:07 +0200
```

यदि आपको अधिक कार्यक्षमता की आवश्यकता है, जैसे कि समय क्षेत्र प्रबंधन, तो आप `ActiveSupport` जैसे तीसरे पक्ष के जेम का उपयोग करना चाहेंगे (Rails का हिस्सा लेकिन अकेले उपयोग किया जा सकता है)।

पहले, अपने Gemfile में `activesupport` जोड़ें और `bundle install` चलाएँ:

```ruby
gem 'activesupport'
```

फिर, समय क्षेत्र संभालने के लिए इसका उपयोग करें:

```ruby
require 'active_support/time'

Time.zone = 'Eastern Time (US & Canada)'  # अपना वांछित समय क्षेत्र सेट करें
current_time_with_zone = Time.zone.now
puts current_time_with_zone
```

नमूना आउटपुट:
```
Wed, 12 Apr 2023 08:33:07 EDT -04:00
```
