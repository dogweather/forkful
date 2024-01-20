---
title:                "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
html_title:           "C#: बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
HTTP अनुरोध के साथ मूल प्रमाणीकरण एक तरीका है जिसका उपयोग सुरक्षित रूप से डेटा को सर्वर में भेजने के लिए किया जाता है। प्रोग्रामर इसे इसलिए करते हैं क्यूंकि यह एक व्यावहारिक और सुरक्षित तरीका है डेटा का संचार करने का।

## कैसे करें:
Gleam में, आप `httpc` library का उपयोग करके Basic Auth के साथ HTTP अनुरोध भेज सकते हैं। यहाँ एक उदाहरण है:

```Gleam
import gleam/httpc.{get, Response}
import gleam/string.concat
import gleam/uri.{Uri, from_string}

fn basic_auth_header(user: String, password: String) -> String {
  "Basic " 
  |> concat(base64.encode(concat(user, ":" |> concat(password))))
}

fn main(uri: Uri) -> Result(Response, Nil) {
  let headers = httpc.default_headers()
    |> list.append([#("Authorization", basic_auth_header("user", "password"))])
  
  get(from_string("https://example.com") |> result.unwrap, headers)
}
```

जब आप इस कोड को चलाते हैं, तो आपका HTTP अनुरोध Basic Auth के साथ `https://example.com` पर भेजा जाएगा। 

## गहन अध्ययन:
HTTP Basic Authentication का इतिहास माइक बर्नर ली की HTTP/1.0 विनिर्देशन में जब शामिल किया गया था, यह आरंभ किया। यह सरल एपीआई है जिसमें उपयोगकर्ता नाम और पासवर्ड का एक single string base64 encoded होता है।

विकल्प तकनीकें OAuth और Digest Access Authentication हैं - दोनों अधिक सुरक्षित हैं लेकिन उन्हें लागू करना अधिक कठिन है।

Basic Auth HTTP अनुरोध में Authentication header के रूप में एक प्रमाणीकरण string शामिल करके काम करता है। कोई भी सर्वर जो Basic Auth समर्थन करता है इस header को decode कर पासवर्ड की पुष्टि कर सकता है। 

## देखें भी:
1. RFC 2617, HTTP Authentication: [Basic and Digest Access Authentication](https://tools.ietf.org/html/rfc2617)
2. Gleam प्रशासनिक [दस्तावेज़ीकरण](https://gleam.run/getting-started/)