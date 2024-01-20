---
title:                "CSV के साथ काम करना"
html_title:           "Bash: CSV के साथ काम करना"
simple_title:         "CSV के साथ काम करना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
CSV (Comma-Separated Values) फ़ाइलें डाटा स्टोर और ट्रांसफर करने का एक सरल तरीका हैं। प्रोग्रामर्स इन्हें टेब्युलर डाटा के सरलतम स्वरूप में पढ़ने, बनाने और संपादित करने के लिए प्रयोग करते हैं।

## How to (कैसे करें):
यहाँ Ruby की मदद से CSV फ़ाइल को कैसे हैंडल किया जाता है, इसके कुछ उदाहरण दिए गए हैं:

- CSV फ़ाइल पढ़ना:
```Ruby
require 'csv'

# Sample CSV file path
file_path = 'example.csv'

# Reading a CSV file
CSV.foreach(file_path, headers: true) do |row|
  puts row.to_hash
end
```

- CSV फ़ाइल लिखना:
```Ruby
require 'csv'

# Sample CSV file path
file_path = 'example_output.csv'

# Writing to a CSV file
CSV.open(file_path, 'wb') do |csv|
  csv << ['Name', 'Age', 'City']
  csv << ['Rahul', '30', 'Delhi']
  csv << ['Priya', '25', 'Mumbai']
end
```

- CSV डाटा को बदलना:
```Ruby
require 'csv'

# Reading and modifying a CSV file and then saving it
input_file_path = 'example.csv'
output_file_path = 'modified_example.csv'

CSV.open(output_file_path, 'wb', headers: true) do |csv|
  CSV.foreach(input_file_path, headers: true) do |row|
    row['Age'] = row['Age'].to_i + 1  # Updating age by adding 1
    csv << row
  end
end
```

## Deep Dive (गहरी जानकारी):
CSV प्रारूप डाटा का एक साधारण और आमतौर पर समझा जाने वाला प्रारूप है, जिसे 1970 के दशक से ही कंप्यूटिंग में प्रयोग किया जा रहा है। CSV फाइलें बिना किसी प्रोग्रामिंग भाषा के भी मानव-पठनीय होती हैं और इन्हें टेक्स्ट एडिटर्स या एक्सेल जैसे प्रोग्रामों में आसानी से खोला जा सकता है। 
Ruby में 'CSV' लाइब्रेरी का प्रयोग करके CSV संरचनाओं के साथ काम करना सरल हो जाता है।
वैकल्पिक रूप से, JSON और XML जैसे अन्य प्रारूपों का प्रयोग करते हुए भी डाटा को संग्रहित और साझा किया जा सकता है, लेकिन जब सरल टेब्युलर डाटा की बात आती है तो CSV का उपयोग अधिक प्रचलित है।

## See Also (यह भी देखें):
रूबी डाक्यूमेंटेशन फॉर CSV: [Ruby's CSV documentation](https://ruby-doc.org/stdlib-2.6.1/libdoc/csv/rdoc/CSV.html)
CSV के बारे में विस्तृत जानकारी के लिए: [RFC 4180](https://tools.ietf.org/html/rfc4180)
Ruby के साथ काम करते समय अन्य डेटा-पार्सिंग लाइब्रेरीज़: [Nokogiri for XML](https://nokogiri.org/), [Roo for Excel files](https://github.com/roo-rb/roo)