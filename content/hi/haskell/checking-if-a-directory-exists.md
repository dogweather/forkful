---
title:                "डायरेक्टरी मौजूद है या नहीं जांचना"
html_title:           "Elm: डायरेक्टरी मौजूद है या नहीं जांचना"
simple_title:         "डायरेक्टरी मौजूद है या नहीं जांचना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

डायरेक्ट्री मौजूद है या नहीं, इसे जांचना मतलब है कि एक निर्दिष्ट पथ पर फ़ोल्डर मौजूद है या नहीं। प्रोग्रामर इसे क्लाउड सिंक्रोनाइज़ेशन, फ़ाइल कोलेज, आदि में फ़ाइलों को सुरक्षित रखने के लिए करते हैं।

## कैसे करें:

### डिपेंडेंसी इंस्टाल करें

```Haskell
stack install directory
```

### कोड उत्कृष्टता

```Haskell
import System.Directory (doesDirectoryExist)

main = do
  print =<< doesDirectoryExist "/path/to/directory"
```

सरल और सीधे है। इसे चलाने पर, यह वापस देगा `True` अगर डायरेक्ट्री मौजूद है, या `False` अगर ग़ैर-मौजूद।

## गहराई में जानकारी

इस कार्य की आवश्यकता संगठन में फ़ाइल सिस्टम की सुसंगतता और सुव्यवस्था के लिए होती है। कभी-कभी प्रोग्रामर ग़लत पथ के साथ कोड को चला देते हैं - अतः डायरेक्ट्री की जांच शायद उस संभावित त्रुटि को रोक सके। 

वैकल्पिक तरीके में, आप `System.Directory.doesPathExist` का उपयोग कर सकते हैं जो फ़ाइल या डायरेक्ट्री दोनों की जांच करेगा।  

इस कार्य की क्रियान्वयन विवरण: `doesDirectoryExist` हास्केल की `directory` पैकेज का हिस्सा है जो POSIX और Windows प्लेटफ़ॉर्म के लिए निर्देशिका की जांच करने की सुविधाएँ प्रदान करता है। 

## देखें भी

अधिक विवरण के लिए, यहां "Haskell Directory" package दस्तावेज़ीकरण देखें: 
[https://hackage.haskell.org/package/directory](https://hackage.haskell.org/package/directory)

जांचें अगर डायरेक्ट्री मौजूद है आदान-प्रदान संवाद यहाँ:
[https://stackoverflow.com/questions/8502201/check-if-directory-exists-in-haskell](https://stackoverflow.com/questions/8502201/check-if-directory-exists-in-haskell)