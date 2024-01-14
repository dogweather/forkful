---
title:                "Gleam: एक अस्थायी फ़ाइल बनाना"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

यदि आप एक हिन्दी ब्लॉगर हैं और सॉफ्टवेयर प्रोग्रामिंग में रुचि रखते हैं, तो आपने Gleam प्रोग्रामिंग भाषा के बारे में जरूर सुना होगा। इस भाषा का प्रयोग करके आप एक अभिनव और शक्तिशाली भाषा सीख सकते हैं जो फंक्शनल और अलग होती है। इस ब्लॉग पोस्ट में हम आपको बताएंगे कि आप किस प्रकार से Gleam में temporary file बना सकते हैं।

## क्यों?

किसी भी काम को सम्पन्न करने के लिए temporary file बनाना बहुत जरूरी हो सकता है। जैसे कि कोई भी बड़ा सॉफ्टवेयर या एप्लिकेशन बनाने के लिए, आपको किसी भी तरह के मुख्य संरचना या डेटा को संचित रखने की आवश्यकता हो सकती है। इसलिए, आपको इस सॉफ्टवेयर को चलाने के लिए किसी अन्य संरचना के साथ काम करना होगा जिससे कि आपको इसे अधिक सरल बनाने में मदद मिले।

## कैसे करें?

आप अपने Gleam प्रोग्राम में temporary file बनाने के लिए निम्नलिखित कार्यवाही का पालन कर सकते हैं:

```Gleam
import gleam/file

// Temporary file बनाने के लिए नया फ़ाइल निर्माण करें
let result = file.Temporary.create()
let temp_file = result.value

// फ़ाइल को संचित करने के लिए एक फाइल पथ बनाएं
let file_path = temp_file.path()

// अपने फ़ाइल में डेटा लिखें
file.File.write(file_path, "मेरा temporary file!")

// फ़ाइल को बनाने के लिए इस्तेमाल किए गए फ़ाइल पथ को छोड़ दें
temp_file.cleanup()
```

जैसे ही आप इस कोड को चलाएंगे, आपको नया temporary file बनाने के लिए एक फाइल प