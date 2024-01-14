---
title:                "Python: HTML को खोजना"
simple_title:         "HTML को खोजना"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/parsing-html.md"
---

{{< edit_this_page >}}

आपने अपने नये प्रोग्रामिंग यात्रा की शुरुआत पाइथन से की है, लेकिन क्या आपने कभी HTML पेज को पार्स करने के बारे में सोचा है? शायद आपको नहीं पता है, लेकिन HTML पेजो को पार्स करने से आपको यह जानने में मदद मिल सकती है कि पेज कैसा है, उसमे क्या काम है और कैसा लगेगा। यह भी आपको एप्पल शिकार के साथ कुछ बेहतरीन कोडिंग क्षमता प्राप्त करने में मदद करेगा।

आपने बहुत सारे साधन पायथन में रूपांतरित कर लिए है, अब आओ बात करते है पाइथन में HTML पेज पार्सिंग कैसे किया जाता है। सारे प्रोग्रामिंग कार्य हम के साथ साझा करिएगा - आएँ पाइथन में HTML पेज पार्सिंग करते हैं:

```python
# पेज केएक्सपीएलटी जेकोड करें
import requests
from bs4 import BeautifulSoup

# पेज का यूआरएल ढूँढें
url = "https://www.example.com"

# पेज से डेटा प्राप्त करें
page_content = requests.get(url).content

# पेज को सूचकांकित करें
soup = BeautifulSoup(page_content, "html.parser")

# ऊपर के हिस्से को फ़िल्टर करें
title = soup.find("h1").text
description = soup.find("p").text

# डेटा को प्रिंट करें
print("Title: " + title)
print("Description: " + description)
```

अगर आप इस कोड को अपने आप से तयार करी हो तो आपको पेज पर शीर्षक जैसा कोई भी शेर कर दिया जाएगा। जैसे कि पेज पर "नमस्ते किसी कोडर!" है, तो आपके कोड का आराम से इस तरह होना चाहिए:

```
Title: नमस्ते किसी कोडर!
```

छूटे हुए प्रिंट पेज पर शीर्षक की तुलना में आपको पी शीर्षक से इस आराम को हमारे नंदि से प्राप्त ह