---
title:                "एक टेक्स्ट फाइल लिखना"
html_title:           "Java: एक टेक्स्ट फाइल लिखना"
simple_title:         "एक टेक्स्ट फाइल लिखना"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

# क्यों

टेक्स्ट फ़ाइल लिखने में क्यों लिखने वाले को रुचि हो सकती है, यह जानने के लिए आइए हम एक सामान्य आधार पर जान लेते हैं कि क्यों टेक्स्ट फ़ाइल जरूरी हो सकती है। सबसे आम स्थान पर, यह उपयोगकर्ता को अपने डेटा को संग्रहीत करने और अद्यतन करने की अनुमति देती है।

# कैसे करें

```Java

// पहले हम टेक्स्ट फ़ाइल बनाएं
File file = new File("फ़ाइल_का_नाम.txt");

// अब हम FileWriter अभिव्यक्ति का उपयोग करके फ़ाइल में लिखें
FileWriter writer = new FileWriter(file);
writer.write("हैलो दुनिया!");
writer.close();

// फ़ाइल सफलतापूर्वक बन गई है!

```

### आउटपुट:

फ़ाइल का नाम.txt
हैलो दुनिया!

# गहराई तक जाएँ

टेक्स्ट फ़ाइल लिखने के दौरान, आप अपने कोड में विभिन्न ऑप्शन भी जोड़ सकते हैं। यह ऑप्शन फ़ाइल को कौन सा ऑब्जेक्ट बनाएं, फ़ाइल में कौन से लाइन लिखें और फ़ाइल से डेटा कैसे पढ़ें जैसी महत्वपूर्ण जानकारी प्रदान कर सकते हैं। आप अपनी फ़ाइल के साथ भी निर्देशित कर सकते हैं, जैसे कि UTF-8 अथवा ASCII कोडिंग का उपयोग करना।

## देखें भी

- [जावा FileWriter डॉक्यूमेंटेशन](https://docs.oracle.com/javase/10/docs/api/java/io/FileWriter.html)
- [जावा फाइल और फ़ाइल पढ़ने के लिए कक्षाएं](https://www.w3schools.com/java/java_files.asp)
- [जावा की आधारभूत हिंदी सीखने का मार्गदर्शन](https://www.journaldev.com/11/learn-java-hindi-part-1-introduction)