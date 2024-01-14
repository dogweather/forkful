---
title:                "Java: वेब पेज डाउनलोड करना"
simple_title:         "वेब पेज डाउनलोड करना"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

बहुत से लोग इन्टरनेट पर वेब पेजों को डाउनलोड करना चाहते हैं, पर वे ये जानना चाहते हैं की आखिर में ये काम किस तरह से होता है।

## क्यों

वेब पेज डाउनलोड करने के कई लाभ हैं। कुछ लोग निरंतर डेटा संग्रह करने के लिए वेब स्क्रेपिंग टूल का उपयोग करते हैं, जबकि कुछ लोग डाउनलोडेड वेब पेज का उपयोग ऑफलाइन उपयोग के लिए करते हैं। अतः एक अच्छा जानकार होना आवश्यक है।

## कैसे करे

जावा में वेब पेज डाउनलोड करने के लिए, हम ये चार कदम फोलो कर सकते हैं:

1. फर्स्ट, हम वेब पेज का URL या लिंक प्राप्त करेंगे।
2. दूसरा, हम URL का उपयोग करके HTTP कन्नेक्शन बनाएंगे।
3. थर्द, हम डाउनलोडेड पेज को स्ट्रिंग में कन्वर्ट करेंगे।
4. फोर्थ, हम उपयोगकर्ता द्वारा दिए गए फाइल नाम से फ़ाइल में डाउनलोड करेगें।

आगे कोड दिखा रहा है कि कैसे हम वेब पेज को डाउनलोड कर सकते हैं:

```Java
import java.net.*;
import java.io.*;

public class DownloadPage {
    public static void main(String[] args) throws Exception {
        // URL बनाएं
        URL url = new URL("https://example.com");
        
        // HTTP कन्नेक्शन बनाएं
        HttpURLConnection connection = (HttpURLConnection) url.openConnection();
        connection.setRequestMethod("GET");
        
        // स्ट्रिंग में पेज को कन्वर्ट करें
        BufferedReader in = new BufferedReader(new InputStreamReader(connection.getInputStream()));
        String inputLine;
        StringBuilder content = new StringBuilder();
        
        while ((inputLine = in.readLine()) != null) {
            content.append(inputLine);
        }
        in.close();
        
        // फ़ाइल में पेज डाउनलोड करें
        String fileName = "example.html";
        FileWriter writer = new FileWriter(fileName);
        writer.write(content.toString());
        writer.close();
        
        // सफलतापूर्वक डाउनलोड हो गया
        System.out.println("वेब पेज सफलतापूर्वक डाउ