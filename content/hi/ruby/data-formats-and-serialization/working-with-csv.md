---
title:                "CSV के साथ काम करना"
aliases:
- /hi/ruby/working-with-csv.md
date:                  2024-02-03T19:22:24.108588-07:00
model:                 gpt-4-0125-preview
simple_title:         "CSV के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

रूबी में CSV फाइल्स के साथ काम करने का तरीका सारणीय डेटा को संभालने के लिए एक सरल दृष्टिकोण प्रदान करता है। प्रोग्रामर्स अक्सर इस प्रथा को डेटा पार्सिंग, निकालने, परिवर्तन, और संग्रहण हेतु अपनाते हैं, जो डेटा मेनिपुलेशन या विश्लेषण में शामिल कार्यों के लिए एक महत्वपूर्ण कौशल बनाता है।

## कैसे:

रूबी में CSV लाइब्रेरी डिफ़ॉल्ट रूप से शामिल होती है, जो CSV फाइल्स से पढ़ने और उनमें लिखने को सरलीकृत करती है। यहाँ आप कैसे सामान्य कार्यों के लिए इसका लाभ उठा सकते हैं:

### एक CSV फाइल पढ़ना
एक CSV फाइल से पढ़ने के लिए, पहले आपको CSV लाइब्रेरी की आवश्यकता होती है। फिर, आप पंक्तियों पर इटरेट कर सकते हैं या उन्हें एक ऐरे में पढ़ सकते हैं।

```ruby
require 'csv'

# प्रत्येक पंक्ति को एक ऐरे के रूप में पढ़ना
CSV.foreach("data.csv") do |row|
  puts row.inspect
end

# प्रत्येक पंक्ति के लिए आउटपुट इस तरह दिख सकता है: ["data1", "data2", "data3"]
```

### एक CSV में लिखना
एक CSV फाइल में लिखना भी सरल है। आप एक मौजूदा फ़ाइल में जोड़ सकते हैं या लिखने के लिए एक नई फ़ाइल बना सकते हैं।

```ruby
require 'csv'

CSV.open("output.csv", "wb") do |csv|
  csv << ["header1", "header2", "header3"]
  csv << ["value1", "value2", "value3"]
end

# इससे 'output.csv' को निर्धारित हेडर्स और मूल्यों के साथ बनाया या अधिलेखित किया जाता है।
```

### एक CSV स्ट्रिंग का पार्सिंग
कभी-कभी आपको सीधे एक स्ट्रिंग से CSV डेटा पार्स करने की आवश्यकता होती है। यहाँ कैसे:

```ruby
require 'csv'

data = "name,age,city\nJohn Doe,29,New York\nJane Doe,31,Chicago"
csv = CSV.parse(data, headers: true)

csv.each do |row|
  puts "#{row['name']} - #{row['age']} - #{row['city']}"
end

# अपेक्षित आउटपुट:
# John Doe - 29 - New York
# Jane Doe - 31 - Chicago
```

### SmarterCSV का उपयोग करना
अधिक जटिल CSV कार्यों के लिए, `SmarterCSV` गेम एक मूल्यवान उपकरण हो सकता है। पहले, गेम इंस्टॉल करें:

```shell
gem install smarter_csv
```

फिर, आप इसका उपयोग बड़ी फाइलों से निपटने या अधिक सोफिस्टिकेटेड पार्सिंग और मेनिप्युलेशन करने के लिए कर सकते हैं:

```ruby
require 'smarter_csv'

options = {}
data = SmarterCSV.process('large_data.csv', options)

data.each do |hash|
  puts hash.inspect
end

# यह 'large_data.csv' को पढ़ेगा और हेडर्स के आधार पर प्रत्येक पंक्ति को हैश के रूप में आउटपुट करेगा।
```

संक्षेप में, रूबी के बिल्ट-इन CSV लाइब्रेरी और `SmarterCSV` जैसे थर्ड-पार्टी जेम्स के साथ, CSV डेटा के साथ हैंडलिंग के लिए मजबूत समर्थन प्रदान करता है, जिससे कुशल डेटा प्रोसेसिंग और मेनिप्युलेशन कार्यों की अनुमति मिलती है।
