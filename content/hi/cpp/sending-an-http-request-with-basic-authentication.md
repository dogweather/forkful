---
title:                "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
html_title:           "C#: बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# अभिप्रेत और क्यों?

HTTP अनुरोध साधारण प्रमाणीकरण के साथ भेजना मतलब होता है, किसी सर्वर तक एक अनुरोध पहुंचाना जिसमें यूजर नाम और पासवर्ड जैसी पहचान की जानकारी हो। प्रोग्रामर इसे तब करते हैं जब वे किसी सुरक्षित सर्वर तक पहुंचना चाहते हैं जिसे उन्हें पहचानना होता है।

# कैसे करें:

यहां एक उदाहरण है जो स्पष्ट करता है कि आपको HTTP अनुरोध बेसिक प्रमाणीकरण के साथ कैसे भेजना है:

```C++
// आवश्यक लाइब्रेरिज़
#include <cpprest/http_client.h>
#include <cpprest/filestream.h>

using namespace utility;    
using namespace web;        
using namespace web::http;  
using namespace web::http::client;

// अनुरोध को भेजने वाला फ़ंक्शन
void sendRequest(http::method mtd, const string_t& username, const string_t& password) {
    http_client_config client_config;
    client_config.set_credentials(credentials(username, password));
    http_client client(U("http://your-webserver.com"), client_config);
    http_request req(mtd);
    client.request(req).get();
}

int main() {
    sendRequest(methods::GET, U("your-username"), U("your-password"));  
    return 0;
}

```

ऊपर दिए गए कोड में, हमने एक HTTP GET अनुरोध भेजा है, जिसमें यूज़रनेम और पासवर्ड होता है।

# गहरा अध्ययन

1. हायपरटेक्स्ट ट्रांसफर प्रोटोकॉल (HTTP) शुरू से ही जानकारी का आदान-प्रदान करने के लिए इंटरनेट पर सबसे आम माध्यम रहा है।
2. अल्टरनेटिव्स: OAuth, Digest Access Authentication, और Bearer Token जैसी अन्य प्रमाणीकरण स्कीमें हो सकती हैं, लेकिन Basic Authentication इतना साधारण है कि इसे समझना और लागू करना सहज है।
3. बेसिक एउथेंटिकेशन में यूज़रनेम और पासवर्ड आपस में 'ः' से जोड़कर Base64 कोडिंग के माध्यम से एन्कोड होते हैं। यह एक हेडर के रूप में एचटीटीपी अनुरोध में जोडा जाता है।

# देखें भी:

1. [HTTP Basic Authentication](https://tools.ietf.org/html/rfc2617#section-2) - HTTP Basic प्रमाणीकरण के बारे में RFC विवरण।
2. [Cpprestsdk library](https://github.com/microsoft/cpprestsdk) - CppRestSDK का Github repository.
3. [Stack Overflow](https://stackoverflow.com/questions/tagged/cpprestsdk) - अधिक प्रश्नों के लिए CppRestSDK टैग के साथ Stack Overflow.