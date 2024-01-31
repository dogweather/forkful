---
title:                "HTTP अनुरोध भेजना"
date:                  2024-01-20T17:59:05.381085-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP अनुरोध भेजना"

category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

HTTP अनुरोध भेजना वेब सर्वर से सूचना मांगना है। प्रोग्रामर इसे डेटा पाने, वेब सर्विसेज के साथ बातचीत करने और APIs का उपयोग करने के लिए करते हैं।

## How to: (कैसे करें:)

आइए `curl` कमांड का इस्तेमाल करके एक HTTP GET अनुरोध भेजते हैं:

```Bash
# HTTP GET अनुरोध
curl https://api.example.com/data

# उदाहरण आउटपुट
{"name":"Example","description":"A sample response from the API."}
```

अब POST अनुरोध प्रयोग करते हैं:

```Bash
# HTTP POST अनुरोध
curl -d "param1=value1&param2=value2" -X POST https://api.example.com/data

# उदाहरण आउटपुट
{"status":"success","message":"Data was posted successfully."}
```

## Deep Dive (गहराई में जानकारी)

HTTP अनुरोध 1990 के दशक से वेब का आधार हैं। `curl` से पहले, Telnet या कस्टम स्क्रिप्ट्स का इस्तेमाल होता था। `wget`, `httpie` जैसे अल्टरनेटिव्स भी मौजूद हैं, लेकिन `curl` इसकी सादगी और व्यापकता के लिए प्रसिद्ध है। `curl` में अनुरोध मेथड, हेडर्स और डेटा को कस्टमाइज़ करने की क्षमता होती है, जो डेवलपर्स को परिष्कृत HTTP अनुरोध भेजने की अनुमति देती है।

## See Also (और जानकारी के लिए)

- `curl` मैनुअल: https://curl.se/docs/manpage.html
- HTTPie (एक विकल्प): https://httpie.io/
- `wget`: https://www.gnu.org/software/wget/manual/wget.html
