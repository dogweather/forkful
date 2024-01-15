---
title:                "बेसिक प्रमाणीकरण के साथ एक http अनुरोध भेजना।"
html_title:           "Elixir: बेसिक प्रमाणीकरण के साथ एक http अनुरोध भेजना।"
simple_title:         "बेसिक प्रमाणीकरण के साथ एक http अनुरोध भेजना।"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्यों

बेसिक प्रमाणीकरण के साथ एचटीटीपी अनुरोध भेजने में कोई क्या लाभ हो सकता है? दूसरे शब्दों में, एक उपयोगकर्ता किस प्रकार से इस प्रक्रिया से लाभान्वित हो सकता है?

## कैसे करें

उपकरण- एलिक्सीर अौर HTTPoison लाइब्रेरी। [ऊपरी स्तम्भकों और पॉइजॉन-पोस्ट रचनाओं के साथ प्रतिवादी मूल्य को लैब्र्री सिर्फ स्ट्रिंगों के लिए, लेरुतन के लिए।](https://hexdocs.pm/httpoison/HTTPoison.Response.html)

कोड:

```
alias HTTPoison.{Base, Request}
import Base64

headers = [
  {"Authorization", "Basic " <> Base.encode64("username:password")}
]

response = HTTPoison.get("https://example.com", [], headers)

IO.inspect response.status # 200
IO.inspect response.body # "<!DOCTYPE html>\n<html>\n<head>\n<title>Welcome to my website</title>\n</head>\n<body>\n<h1>Hello World</h1>\n</body>\n</html>"
```

## गहरी खोज करें

यह कैसे काम करता है? अपने साइट को पॉप से बीपीएन भेजने के लिए चाहते हो तो मैं चाहता हूँ कि आप सही तरह से स्थापित करती हो। अन्यथा, आप अपने हेडर्स मे स्पष्ट भाषा का हाॅड छान सकते हो। अन्यथा, [पॉइजॉन योजना की राह को पढ़कर आप देख सकते है कि दाग के पदों को जोड़ मूनशा लोगो चप्पे-चप्पे चोट पहारा सकते हो](http://gigerdone.com/2017/11/19/httpoison-plan.html).

घोर में जाने से पहले एक बार, यह सुनिश्चित करने के लिए आपको होगा कि आपका सर्भर बॉर्ड ताकि आपका नाम और शब्द सही स्थान पर लिजे गए हैं। वैध ग्राहक का एक नाम और शब्द कॉल कर सकते हो।

## देखें भी

- [वैध हेडर सीमा की अनुपात जांच](https://www.rfc-editor.org/rfc/rfc7235#section-2.1)
- [प