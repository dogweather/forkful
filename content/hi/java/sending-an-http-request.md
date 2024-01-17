---
title:                "http अनुरोध भेजना"
html_title:           "Java: http अनुरोध भेजना"
simple_title:         "http अनुरोध भेजना"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्या है और क्यों?
HTTP अनुरोध भेजना एक प्रोग्रामर की डेवलपमेंट में एक आवश्यक क्रिया है। यह एक सरल प्रक्रिया है जो सभी अनुरोधों को डाउनलोड करने और सर्वर द्वारा प्रतिसाद लेने की अनुमति देती है। अधिकांश ब्राउज़र राउटिंग, फॉर्म भरने, डाउनलोडिंग फ़ाइल्स जैसे कार्यों के लिए HTTP अनुरोधों का उपयोग करते हैं।

## कैसे करें:
ब्राउज़र डाउनलोडिंग या अन्य कार्यों को करने के लिए HTTP अनुरोधों का उपयोग करता है। आपको अपने अनुरोध को बाहरी सेवा या सर्वर को भेजने के लिए सार्वजनिक आईपी या डोमेन नाम के साथ URL के साथ पहुंच करने के लिए HttpURLConnection ऑब्जेक्ट को बनाना पड़ेगा। उदाहरण के लिए:

```Java
URL url = new URL("http://www.example.com");
HttpURLConnection con = (HttpURLConnection) url.openConnection();
con.setRequestMethod("GET");
System.out.println(con.getResponseCode());
```

उपरोक्त कोड सर्वर से जवाब को लाने के लिए ब्राउज़र निर्देशित करेगा। अगर सफलतापूर्वक वह एक आपको यूआरएल के मुख्य भाग में सर्वर से संबंध रखने के लिए आपकी वेबकासुरथा से संबंधित उत्तर को पेश करेगा।

## Deep Dive:
HTTP या हाइपर टेक्स्ट ट्रांस्फर प्रोटोकॉल एक एप्पलिकेशन लेयर प्रोटोकॉल है जो इंटरनेट पर संचार को संभव बनाता है। यह सीमित रूप से निर्वाचित डेटा पर आयात और निर्यात करने का प्रमुख माध्यम है। HTTP का प्रयोग आमतौर पर वेब सेवा कॉल, रीडिंग डॉक्यूमेंट और डाटा को अन्य स्रोतों से डाउनलोड करने के लिए किया जाता है। अन्य विकल्पों में AJAX, WebSockets, और उदहारणात्मक अनुरोधों का उपयोग किया जाता है। HttpURLConnection एपीआई और Apache HttpComponents प्रकार के लाइब्रेरी HTTP अनुरोध भेजने के लिए उपयोगी हैं।

## See Also:
- [Oracle डॉक्यूमेंटेशन: HttpURLConnection](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html)
- [Apache HttpComponents](https://hc.apache.org/)