---
title:                "एक वेब पृष्ठ डाउनलोड करना"
html_title:           "Java: एक वेब पृष्ठ डाउनलोड करना"
simple_title:         "एक वेब पृष्ठ डाउनलोड करना"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
जावा एक उचित आवृत्ति में किसी वेब पृष्ठ को डाउनलोड करना है क्या है और इसे प्रोग्रामर्स क्यों करते हैं।

## कैसे करे:
जावा में वेब पृष्ठ को डाउनलोड करने के लिए हम एक रेडर क्लास बना सकते हैं जो URL को प्राप्त करेगा और उसे स्ट्रिंग में अनामित करेगा। उसके बाद हम स्ट्रिंग को फाइल में लिख सकते हैं। निम्न दिए गए कोड ब्लॉक में एक उदाहरण दिया गया है:
```Java
try{
    URL url = new URL("https://www.example.com/");
    BufferedReader reader = new BufferedReader(new InputStreamReader(url.openStream()));

    String line;
    while ((line = reader.readLine()) != null) {
        System.out.println(line);
    }
    reader.close();
} catch (IOException e) {
    e.printStackTrace();
}
```
यहां, हम वेब पृष्ठ से प्रत्येक पंक्ति को प्रिंट करते हैं।

## गहराई में जाएं:
इतिहासी परिप्रेक्ष्य, वैकल्पिक उपाय और वेब पृष्ठ को डाउनलोड करने के लिए आवश्यक तरीकों जैसे कि HTTP कनेक्शन, कुकी एवं संस्करण जानने के बाद आप स्वयं अपनी कोड में और आसानी से वेब पृष्ठों को डाउनलोड कर सकते हैं।

## देखें भी:
अधिक जानकारी के लिए, आप निम्न लिंक्स का उपयोग कर सकते हैं:
- जावा डॉक्यूमेंटेशन: <https://docs.oracle.com/javase/10/docs/api/java/net/URL.html>
- जावा इमेज को मेमोरी में डाउनलोड करना: <https://www.geeksforgeeks.org/downloading-image-web-using-java/>
- जावा कोड में वेब पृष्ठों को डाउनलोड करना: <https://mkyong.com/java/how-to-download-file-from-website-java-jsp/>