---
title:                "Elm: डायरेक्टरी मौजूद है की जाँच करना"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## क्यों

दिनचर्या के दौरान हम अक्सर फ़ाइल या फ़ोल्डर की उपस्थिति की जाँच करते हैं, लेकिन क्या आपने कभी सोचा है कि आप अपने Elm प्रोग्राम में भी इसे कैसे कर सकते हैं? इस ब्लॉग पोस्ट में हम आपको बताएंगे कि Elm में निर्देशित करें कि फ़ोल्डर या डायरेक्टरी मौजूद है या नहीं।


## कैसे करे

फ़ोल्डर या डायरेक्टरी की उपस्थिति की जाँच करने के लिए, हम आसानी से निम्नलिखित मेथड का उपयोग कर सकते हैं:

```Elm
import Task exposing (Task)
import File

isDirectoryExists : String -> Task Never Bool
isDirectoryExists path =
  File.exists path

```

ऊपर दिए गए उदाहरण में, हमने `Task` मॉड्यूल से `Task Never Bool` टाइप का एक मेथड `isDirectoryExists` बनाया है जो कि फ़ोल्डर या डायरेक्टरी की जाँच को असिस्तेड करता है। उसके साथ हमने Elm के `File` मॉड्यूल से `exists` फ़ंक्शन को कॉल किया है जो कि दिए गए पथ पर फ़ोल्डर या डायरेक्टरी की मौजूदगी की जाँच करता है।

अब हम इस मेथड का उपयोग करके फ़ोल्डर या डायरेक्टरी की जाँच कर सकते हैं, जैसे ही उपयोगकर्ता इसे दर्ज करेंगे:

```Elm
isDirectoryExists "documents/"

-- Output:
-- If "documents/" directory exists: Task.Ok True
-- If "documents/" directory doesn't exist: Task.Ok False
```

ऊपर दिए गए उदाहरण में, यदि "documents/" नामक फ़ोल्डर मौजूद होता है तो हमें टास्क की सफलता सकारात्मक `True` मिलेगा और अगर यह मौजूद नहीं होता है तो प्रस्तुति रूप में उपयोगकर्ता को `False` की तरह रिटर्न क