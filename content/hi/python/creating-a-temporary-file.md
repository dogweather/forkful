---
title:                "Python: अस्थायी फाइल बनाना"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## इसका कारण

आधुनिक डाटा साइंस एप्लीकेशन्स को भी अस्थायी फ़ाइलें बनाने की आवश्यकता होती है जिससे वे सिस्टम स्टोरेज को ख़ास रूप से लोड ना कर पाएं और ऐसे फ़ाइलें प्राप्त होने के पश्चात डिलीट हो सकें।

## कैसे करें

```Python
import tempfile

# अस्थायी फ़ाइल बनाएं
temp_file = tempfile.TemporaryFile()

# अस्थायी फ़ाइल में कुछ डेटा लिखें
temp_file.write(b"हैलो दुनिया!")

# डेटा पढ़ें
temp_file.seek(0)
data = temp_file.read()

# प्रिंट करें
print(data)

# अस्थायी फ़ाइल को डिलीट करें
temp_file.close()
```

आउटपुट:

```
हैलो दुनिया!
```

## गहराई निरीक्षण

अस्थायी फ़ाइलें एक बहुत ही उपयोगी काम करती हैं, ख़ास तौर पर किसी स्थानीय स्टोरेज को न सुनिश्चित करने के लिए। ये एक आसान और एक्स्पर्ट दोनों के लिए ऐसे डेटा साथियों हो सकते हैं जो अस्थायी डेटा बचाना चाहते हैं।

## देखें भी

- [अस्थायी फ़ाइल से सम्बंधित पायथन डॉक्यूमेंटेशन] (https://docs.python.org/3/library/tempfile.html)
- [डेटा साइनस में अस्थायी फाइलें का उपयोग करने का आँखफोलना स्टाइल] (https://towardsdatascience.com/using-temporary-files-in-python-190cb12f25a4)
- [डेटा साइंस को शुरू करने के लिए पायथन में अस्थायी फ़ाइलें का उपयोग करना] (https://realpython.com/python-tempfile/)