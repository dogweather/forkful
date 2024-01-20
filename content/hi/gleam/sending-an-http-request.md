---
title:                "http अनुरोध भेजना"
html_title:           "Elixir: http अनुरोध भेजना"
simple_title:         "http अनुरोध भेजना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

HTTP अनुरोध भेजना (HTTP Request Sending) एक प्रोग्रामिंग क्रिया है जिससे वेब सर्वर से डाटा का विनिमय होता है। प्रोग्रामर्स इसे वेबसाइटों से जानकारी प्राप्त करने, या उनपर परिवर्तन करने के लिए करते हैं।

## कैसे करें:

आइए देखते हैं कि Gleam में HTTP अनुरोध कैसे संपन्न किया जाता है।

```Gleam
आयात gleam/httpc

फ़ंक्शन send_request() {
    httpc.get("https://api.github.com/users/octocat")
        |> लिफ्ट(Result.map(तर्क |resp| resp.body))
}
```

ऊपर दिए गए कोड में, `httpc.get` फ़ंक्शन का उपयोग करके Github API से HTTP GET अनुरोध भेजा जाता है। इसका परिणाम HTTP प्रतिक्रिया के रूप में होगा, जिसके शरीर का प्राप्ति करने के लिए `resp.body` संबोधित किया जाएगा।

## गहरी जांच

HTTP अनुरोध 1990 के शुरुआती दिनों से ही वेब प्रोग्रामिंग का मौलिक हिस्सा रहा है, जब WWW (World Wide Web) का निर्माण हुआ था।

HTTP अनुरोध के विकल्प शामिल हैं WebSocket, EventSource, SCP, FTP, और बहुत कुछ, पर हर एक का अपना उद्देश्य एवं उपयोग है।

Gleam में HTTP अनुरोध का कार्यान्वयन Erlang संगठनात्मक यूनिट, OTP (Open Telecom Protocol) के उपयोग से होता है। इसका मुख्य लक्ष्य अन्वेषण, श्रृंखलाबद्धता और समरुद्धिता को बढ़ाना है।

## अन्य संदर्भ स्रोत

- [Gleam डॉक्युमेंटेशन के HTTP अनुभाग](https://gleam.run/book/tour/http-requests.html)
- [Erlang OTP डॉक्युमेंटेशन](https://erlang.org/doc/man/OTP.html)
- [HTTP: ये हाइपरटेक्स्ट ट्रांसफर प्रोटोकॉल क्या है](https://www.w3schools.com/whatis/whatis_http.asp)