---
title:                "Gleam: बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# क्यों

एचटीटीपी अनुरोध भेजने में मूलभूत प्रमाणीकरण के साथ होने का कारण केवल 1-2 वाक्य हो सकता है।

# कैसे करें

```Gleam
import gleam/http
import gleam/pipeline/http

let response =
  http.request(
    url = "https://example.com",
    auth = http.basic_auth(username = "my_username", password = "my_password")
  )

response.body // "Success! You are now authenticated with basic authentication."
```

# गहराई में जाएँ

बेसिक प्रमाणीकरण के साथ एचटीटीपी अनुरोध भेजने के बारे में गहराई में जानकारी।

# देखें भी

- [Gleam डॉक्यूमेंटेशन](https://gleam.run/documentation/)
- [एचटीटीपी आईपी रिक्वेस्ट](https://gleam.run/documentation/stdlib/http.html#request)
- [गहराई में दिलचस्प पड़ता है](https://blog.geod.in/gleam/http-requests-with-gleam/)