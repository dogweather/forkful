---
title:                "Elm: अस्थायी फ़ाइल बनाना"
simple_title:         "अस्थायी फ़ाइल बनाना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्यों

क्या आप ने कभी सोचा है कि क्यों कोई व्यक्ति अस्थायी फ़ाइल बनाने में लग जाता है? एल्म प्रोग्रामिंग में यह एक आम चीज है जो लोग करते हैं, और इस लेख में हम आपको बताएंगे कि इसका मतलब क्या है और ऐसी फाइल कैसे बनाई जाती है।

## कैसे

अस्थायी फ़ाइल बनाने को समझने के लिए, हमें सबसे पहले एक एल्म कोडिंग उदाहरण की आवश्यकता होगी। इसके बाद हम उसका आउटपुट देखेंगे। नीचे दिए गए कोड ब्लॉक को आप अपना एल्म फ़ाइल में कॉपी और पेस्ट करके देख सकते हैं:

```Elm
import File
import Random
import Time

main =
  Time.now
    |> Random.step
    |> File.temp "myTempFile.txt"
  |> Task.attempt handleTaskResult

handleTaskResult result =
  case result of
    Ok fileName ->
      "सफलतापूर्वक एक अस्थायी फ़ाइल बनाई गई है: " ++ fileName
    Err err ->
      "फ़ाइल बनाने में एक समस्या हुई है: " ++ err
```

जैसा कि आप उपर दिए गए कोड से देख सकते हैं, हमने Time, Random और File मॉड्यूल को आयात किया है और एक मुख्य फ़ंक्शन में Time.now और Random.step को combine करके File.temp का उपयोग किया है। इससे हम एक अस्थायी फ़ाइल बना सकते हैं, जिसका नाम एल्म कोड चलाने के साथ-साथ अपने आप बदल जाएगा। इसके साथ ही हमने Task.attempt का भी उपयोग किया है, जो कि एक फ़ंक्शन को कॉल करता है जो handleTaskResult नाम का है, जिसमें हम फ़ाइल नाम को ले सकते हैं और अपने एल्म फ़ाइल में उपयोग कर सकते हैं। अगर सब सम