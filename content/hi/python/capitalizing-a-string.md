---
title:                "Python: स्ट्रिंग को शीर्षकीकृत करना"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# क्यों

बहुत से प्रोग्रामिंग भाषाओं में हमें उपयोगकर्ता के द्वारा दी गई इनपुट को ऑपरेटर या फंक्शन का उपयोग या संदर्भ कैपिटलाइज़ करने के लिए चाहिए होता है। इससे हमारे प्रोग्राम को जटिलता से राहत मिलती है और हमारे कोड को पढ़ने के लिए आसान बनाता है।

## कैसे करें

```
Python
input_str = "Hello World!"
print(input_str.capitalize())
```

उपरोक्त कोड का नतीजा निम्न रूप में होगा:

```
Hello world!
```

## गहराई में जाएं

कैपिटलाइज़ फंक्शन स्ट्रिंग्स में पहले अक्षर को बड़ा करता है और बाकी अक्षरों को छोटा करता है। पाइथन में, यह स्ट्रिंग्स के लिए प्रयोग किया जाता है जिससे स्ट्रिंग का प्रथम अक्षर बड़ा और बाकी अक्षर छोटे हो जाएं।

## देखें भी

- [इनपुट और आउटपुट को पाइथन में प्रिंट कैसे करें](https://medium.com/@pythonhindi/%E0%A4%87%E0%A4%A8%E0%A4%AA%E0%A5%81%E0%A4%9F-%E0%A4%94%E0%A4%B0-%E0%A4%86%E0%A4%89%E0%A4%9F%E0%A4%AA%E0%A5%81%E0%A4%9F-%E0%A4%95%E0%A5%8B-%E0%A4%AA%E0%A4%BE%E0%A4%87%E0%A4%A5%E0%A4%A8-%E0%A4%AE%E0%A5%87%E0%A4%82-%E0%A4%AA%E0%A5%8D%E0%A4%B0%E0%A4%BF%E0%A4%82%E0%A4%9F-%E0%A4%95%E0%A5%88%E0%A4%B8%E0%A5%87-%E0%A4%95%E0%A4%B0%E0%A5%87%E0%A4%82-9ae2bfbc32e3)
- [पाइथन के साथ पाइपेक्टोर क्या होता है?](https://cyber-security.online/blog/python-with-pipekator/)