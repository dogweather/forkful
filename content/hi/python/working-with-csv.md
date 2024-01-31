---
title:                "CSV के साथ काम करना"
date:                  2024-01-19
html_title:           "Bash: CSV के साथ काम करना"
simple_title:         "CSV के साथ काम करना"

category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/working-with-csv.md"
---

{{< edit_this_page >}}

## क्या & क्यों?
Python में CSV (Comma-Separated Values) से काम करने का मतलब है टेक्स्ट फाइल्स में डेटा को पढ़ना और लिखना, जिन्हें आसानी से टेबल्स की तरह समझा जा सकता है। प्रोग्रामर्स इसका उपयोग डेटा एक्सचेंज और स्टोरेज के लिए करते हैं क्योंकि यह सार्वभौमिक तरीका है और आसानी से कई अलग-अलग प्लेटफार्म्स पर पढ़ा जा सकता है।

## कैसे करें:
```Python
# CSV फाइल पढ़ना
import csv

filename = "sample.csv"
with open(filename, mode='r') as file:
    csv_reader = csv.reader(file)
    for row in csv_reader:
        print(row)

# CSV फाइल लिखना
data = [["नाम", "उम्र"], ["राहुल", 30], ["सीमा", 28]]

with open("output.csv", mode='w', newline='') as file:
    csv_writer = csv.writer(file)
    csv_writer.writerows(data)
```

## गहराई से समझ:
CSV का फॉर्मेट सबसे पहले 1970 के दशक में उपयोग में आया था और तब से यह डेटा को स्टोर और एक्सचेंज करने का एक मानक रूप बना हुआ है। वैकल्पिक रूप में, XML और JSON जैसे फॉर्मेट्स भी मौजूद हैं, पर CSV की सरलता और व्यापक स्वीकार्यता इसे लोकप्रिय बनाए रखती है। CSV में सबसे बड़ी कमी यह है कि इसमें डेटा टाइप्स पर स्पष्टता की कमी होती है, और कई संगठित डेटा को इसमें संग्रहित करना मुश्किल होता है।

## और भी देखें:
- Python की आधिकारिक CSV मॉड्यूल डॉक्यूमेंटेशन: https://docs.python.org/3/library/csv.html
- CSV के लिए pandas लाइब्रेरी: https://pandas.pydata.org/pandas-docs/stable/user_guide/io.html#csv-text-files
- CSV फाइल्स के साथ काम करते समय अच्छी प्रैक्टिस: https://realpython.com/python-csv/
