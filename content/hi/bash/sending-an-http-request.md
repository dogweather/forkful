---
title:                "http अनुरोध भेजना"
html_title:           "Elixir: http अनुरोध भेजना"
simple_title:         "http अनुरोध भेजना"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
HTTP से अनुरोध भेजना एक तरीका है किसी वेबसर्वर से डाटा लेने का। यह उस स्थिति में उपयोगी होता है जब प्रोग्रामर्स को बाहरी API से डाटा चाहिए, जैसे हमारी वेबसाइट में किसी अन्य स्रोत से जानकारी प्रदर्शित करना।

## कैसे करें:
Bash प्रोग्रामिंग में, `curl` आदेश हमें HTTP प्रार्थनाओं को भेजने में मदद करता है। उदाहरण के लिए:

```Bash
# GET अनुरोध
curl https://api.github.com/users/octocat
```

इसका परिणाम कुछ इस तरह का होगा: 

```Bash
{
  "login": "octocat",
  "id": 583231,
  ...
}
```
## गहरा डाइव
Bash का उपयोग प्राधिकरण, प्राप्ति, और डाटा प्रसंस्करण के लिए वेब समस्याओं का समाधान करने के लिए किया जाता है। `curl` १९९६ से उपलब्ध है और यह POSIX से सुसंगत शेल भाषा है। 

Bash के विकल्प में Python, Node.js, और Ruby शामिल हैं, जो अधिक मोडर्न और फंक्शनलिटी की दृष्टि से शक्तिशाली होते हैं। आप `requests` (Python), `axios` (JavaScript), और `net::http` (Ruby) का उपयोग करके HTTP अनुरोध भेज सकते हैं।

## अन्य स्रोत देखें
1. [`curl` में HTTP अनुरोधों को भेजने के बारे में अधिक जानकारी के लिए](https://linuxize.com/post/curl-command-examples/)
2. [HTTP प्रार्थनाओं के बारे में अधिक जानने के लिए](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods)
3. [Python, Node.js, और Ruby में HTTP अनुरोधों को भेजने पर विस्तृत मार्गदर्शिका](https://www.smashingmagazine.com/2018/01/understanding-using-rest-api/)