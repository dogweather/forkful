---
title:                "वेब पेज डाउनलोड करना"
aliases:
- /hi/c/downloading-a-web-page.md
date:                  2024-02-03T17:57:11.266236-07:00
model:                 gpt-4-0125-preview
simple_title:         "वेब पेज डाउनलोड करना"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/downloading-a-web-page.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

C में एक वेब पेज को डाउनलोड करने का मतलब इंटरनेट पर एक वेब पेज की सामग्री तक प्रोग्रामैटिक रूप से पहुंचना और इसे प्रोसेसिंग या ऑफलाइन उपयोग के लिए स्थानीय रूप से सहेजना है। प्रोग्रामर अक्सर इसे वेब सेवाओं का उपभोग करने, वेब सामग्री को स्क्रेप करने, या अपने अनुप्रयोगों से सीधे ऑनलाइन संसाधनों के साथ इंटरैक्ट करने के लिए करते हैं।

## कैसे करें:

C में एक वेब पेज को डाउनलोड करने के लिए, एक लोकप्रिय तरीका libcurl लाइब्रेरी का उपयोग करना है, जो एक कुशल और पोर्टेबल क्लाइंट-साइड URL ट्रांसफर लाइब्रेरी है। सुनिश्चित करें कि आपके प्रोजेक्ट में libcurl इंस्टॉल किया गया है और जोड़ा गया है। यहां एक उदाहरण दिया गया है जो दिखाता है कि कैसे libcurl का उपयोग करके वेब पेज की सामग्री को डाउनलोड किया जाए:

```c
#include <stdio.h>
#include <curl/curl.h>

size_t write_data(void *ptr, size_t size, size_t nmemb, FILE *stream) {
    size_t written = fwrite(ptr, size, nmemb, stream);
    return written;
}

int main(void) {
    CURL *curl;
    FILE *fp;
    CURLcode res;
    char *url = "http://example.com";
    char outfilename[FILENAME_MAX] = "./downloaded_page.html";

    curl = curl_easy_init(); // एक libcurl आसान सत्र की शुरूआत करें
    if (curl) {
        fp = fopen(outfilename,"wb");
        curl_easy_setopt(curl, CURLOPT_URL, url);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data); // प्राप्त डेटा को लिखने के लिए कॉलबैक
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp); // डेटा को लिखने के लिए फाइल पॉइंटर सेट करें

        res = curl_easy_perform(curl); // फाइल डाउनलोड करें
        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                    curl_easy_strerror(res));
        }

        /* हमेशा सफाई करें */
        curl_easy_cleanup(curl); // आसान सत्र को साफ करें
        fclose(fp); // फाइल स्ट्रीम को बंद करें
    }
    return 0;
}
```
नमूना आउटपुट (कंसोल में कोई दृश्य आउटपुट नहीं): यह कोड निर्दिष्ट URL पर सामग्री को डाउनलोड करता है और इसे `downloaded_page.html` नामक एक फाइल में सहेजता है। डाउनलोड की गई सामग्री देखने के लिए अपने प्रोग्राम की निर्देशिका की जांच करें।

## विस्तृत चर्चा:

ऐतिहासिक रूप से, C में वेब सामग्री को डाउनलोड करना अधिक कठिन था, जिसमें मैनुअल सॉकेट प्रोग्रामिंग और HTTP प्रोटोकॉल हैंडलिंग की आवश्यकता होती थी। Libcurl इन जटिलताओं को सरल बनाता है, वेब पर डेटा ट्रांसफर के लिए एक मजबूत और उच्च-स्तरीय API प्रदान करता है।

जबकि libcurl C में HTTP अनुरोधों को सरल बनाता है, मॉडर्न प्रोग्रामिंग भाषाएं जैसे कि Python अपने `requests` लाइब्रेरी के साथ या JavaScript (Node.js) विभिन्न HTTP क्लाइंट लाइब्रेरीज़ के साथ, वेब संचार में आमतौर पर उपयोग किए जाने वाले JSON और अन्य डेटा प्रारूपों के लिए अधिक सहज सिंटैक्स और अंतर्निहित समर्थन प्रदान कर सकती हैं। हालांकि, C और libcurl कुशलता, विस्तृत नियंत्रण, या मौजूदा C कोडबेस में एकीकरण के लिए महत्वपूर्ण सिस्टमों के लिए एक उच्च-प्रदर्शन और स्थिर समाधान प्रदान करते हैं। यह भी ध्यान देने योग्य है कि C, libcurl के साथ जोड़कर, केवल वेब पेज डाउनलोड करने के लिए ही नहीं बल्कि FTP, SMTP, और बहुत कुछ के लिए भी उपयोग किया जा सकता है, जिससे यह एक प्रोग्रामर के टूलकिट में एक बहुमुखी उपकरण बन जाता है।
