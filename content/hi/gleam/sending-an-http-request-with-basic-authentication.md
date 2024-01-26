---
title:                "बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना"
date:                  2024-01-20T18:02:24.659832-07:00
model:                 gpt-4-1106-preview
simple_title:         "बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

HTTP अनुरोध भेजना बेसिक प्रमाणीकरण के साथ सर्वर को निर्देश देना होता है ताकि आपको विशेष साइट सुविधाओं तक पहुंच मिल सके। प्रोग्रामर इसे उपयोगकर्ता डेटा की रक्षा और अनधिकृत पहुंच से बचने के लिए करते हैं।

## कैसे करें:

```gleam
import gleam/http
import gleam/httpc
import gleam/string

fn send_auth_request() -> Result(http.Response, http.Error) {
  httpc.send( http.Request(
    method: http.Get,
    url: "http://example.com",
    headers: [http.Header("Authorization", "Basic " ++ string.from_bytes("username:password") |> string.base64)],
  ))
}

pub fn main() {
  case send_auth_request() {
    Ok(response) -> io.println("सफलता: " ++ response.status_code |> int.to_string())
    Error(error) -> io.println("त्रुटि: " ++ error)
  }
}
```

सैंपल आउटपुट:
```
सफलता: 200
```

## गहराई से जानकारी:

बेसिक प्रमाणीकरण में, यूजरनेम और पासवर्ड को कॉलन (`:`) से जोड़ा जाता है, Base64 में एन्कोड किया जाता है, और "Authorization" हैडर में शामिल किया जाता है। यह प्रणाली HTTP/1.0 से है, लेकिन आज भी इस्तेमाल होती है।

बेसिक प्रमाणीकरण सुरक्षित नहीं है क्योंकि Base64 एन्कोडिंग को डिकोड किया जा सकता है। इसके लिए, HTTPS का इस्तेमाल जरूरी है। अल्टरनेटिव्स में Bearer टोकंस, OAuth और Digest प्रमाणीकरण शामिल हैं।

Gleam का `httpc` मॉड्यूल सरल HTTP क्लाइंट है जिसका उपयोग कस्टम HTTP अनुरोध भेजने के लिए किया जाता है।

## संदर्भ के लिए:

- Gleam HTTP documentation: https://gleam.run/book/tour/http-clients.html
- HTTP Authentication: Basic and Digest Access Authentication (RFC 2617): https://tools.ietf.org/html/rfc2617
- Base64 Encoding: https://en.wikipedia.org/wiki/Base64
