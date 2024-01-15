---
title:                "वर्तमान दिनांक प्राप्त करना"
html_title:           "Java: वर्तमान दिनांक प्राप्त करना"
simple_title:         "वर्तमान दिनांक प्राप्त करना"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## लोग आजकल क्यूरेंट डेट को प्राप्त करने में उलझे हुए हैं। 

शायद यह जानने के लिए कि वर्तमान तिथि क्यों महत्वपूर्ण है और कैसे इसका पता लगाया जाता है। यह एक अत्यंत उपयोगी और आवश्यक कौशल है जो सभी नए जावा प्रोग्रामर्स को सीखना चाहिए।

## कैसे करें 

```Java
// एक इंस्टेंस बनाएं
Date currentDate = new Date();
// फार्मेट को निर्धारित करें
SimpleDateFormat dateFormatter= new SimpleDateFormat("dd/MM/yyyy");
// फार्मेटेड डेट को प्राप्त करें
String formattedDate= dateFormatter.format(currentDate);
// फार्मेट डेट को प्रिंट करें
System.out.println("वर्तमान तिथि: " + formattedDate);
```

### उत्पादन:

```
वर्तमान तिथि: 12/06/2021
```

## गहराई में

जावा में वर्तमान तिथि प्राप्त करना अधिकांश उपयोगी कॉन्स्ट्रक्टर्स में से एक है। एक तारीख और समय वर्ग संख्या से प्राप्त करना और इसे उचित फोर्मेट में प्रदर्शित करना जावा में आसान है। वर्तमान तिथि को प्राप्त करने के लिए, जावा में विभिन्न तरीकों का उपयोग किया जा सकता है, जैसे कि Date, Calendar और SimpleDataFormat का उपयोग किया जाता है।

## देखें भी

- [Java Date मैन्युअल] (https://docs.oracle.com/javase/8/docs/api/java/sql/Date.html)
- [Java Calendar मैन्युअल] (https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)
- [Java SimpleDateFormat मैन्युअल] (https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)