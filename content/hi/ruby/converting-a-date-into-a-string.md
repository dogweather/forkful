---
title:                "एक तारीख को स्ट्रिंग में परिवर्तित करना"
html_title:           "Java: एक तारीख को स्ट्रिंग में परिवर्तित करना"
simple_title:         "एक तारीख को स्ट्रिंग में परिवर्तित करना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों? 

डेट को स्ट्रिंग में कन्वर्ट करना एक ऐसी प्रक्रिया है जिसमें एक डेट ऑब्जेक्ट को एक पठनीय या स्टोर करने योग्य फ़ॉर्म में ट्रांसफ़ॉर्म किया जाता है। यह तब किया जाता है जब प्रोग्रामर्स को डेट को उपयोगकर्ताओं को प्रदर्शित करने, फ़ाइलों में सहेजने या डेटाबेस में डालने की आवश्यकता होती है। 

## कैसे करें:

Ruby में, `strftime` मेथड का उपयोग करके आप डेट को स्ट्रिंग में परिवर्तित कर सकते हैं।

```Ruby
# इस्तेमाल कोड
today = Date.today
str_today = today.strftime("%d/%m/%Y")

puts str_today
# Output: "18/09/2022"
```

यहां `strftime` मेथड का उपयोग करके डेट को दिन/महीना/वर्ष फ़ॉर्मैट में स्ट्रिंग में बदला गया है।

## गहरा डाइव:

`strftime` का नाम "String Format Time" से आता है और इसे 1980 के दशक में Unix पर बनाया गया था। आप `strftime` का उपयोग करके डेट और टाइम को कस्टम फ़ॉर्मैट में बदल सकते हैं।

विकल्पों में, आप का उपयोग कर सकते हैं `to_s` या `iso8601` मेथड विशिष्ट फ़ॉर्मैट में डेट को स्ट्रिंग में परिवर्तित करने के लिए। हालांकि, `strftime` अधिक कस्टमाइज़ेशन की अनुमति देता है।

## देखें भी:

और अधिक अभ्यास और जानकारी के लिए, निम्नलिखित लिंक देखें:

1. [Ruby का आधिकारिक डॉक्यूमेंटेशन](https://www.ruby-lang.org/en/documentation/)
2. [Ruby strftime मेथड](https://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/Date.html#method-i-strftime)
3. [Ruby Date के बारे में सीखें](https://www.rb-path.com/ruby-date.html)