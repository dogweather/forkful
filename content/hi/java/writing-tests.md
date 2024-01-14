---
title:    "Java: प्रोग्रामिंग में टेस्ट लिखना"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/java/writing-tests.md"
---

{{< edit_this_page >}}

##क्यों
तो अपने कोड पर भरोसा कैसे करें? आखिरकार, हर कोडर के लिए उसके कोड के साथ खेलने के बिना अपना समय बर्बाद करना कठिन होता है। इसलिए, अपने कोड को सुरक्षित बनाने के लिए, टेस्टिंग करना बहुत आवश्यक है। यह आपको अपने कोड की सामग्री और एक्सपेक्टेशंस को समझने में मदद करेगा और सुनिश्चित करेगा कि आपका कोड उन चीजों को करता है जो आपने उसे करने के लिए प्रोग्राम किया था।

##कैसे करें
टेस्ट केस कैसे लिखें? चिंता न करें, हम आपको इस प्रकार की फाइल में समझाएंगे और जिंदगी को स्ट्रेस-फ्री बनाएंगे। एक सरल उदाहरण के साथ, हम आपको दिखाएंगे कि आप कैसे कुछ बेहतरीन टेस्ट लिख सकते हैं और अपने कोड को कैसे सुनिश्चित कर सकते हैं। हम इसे एक "यूआरएल कनेक्शन टेस्टर" के साथ देखेंगे, जो दिए गए यूआरएल पर कनेक्शन करता है और सही या गलत यूआरएल होने की जांच करता है।

```Java
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;

public class URLConnectionTester {

    public static void main(String[] args) {
        try {
            //यूआरएल और उम्मीद
            URL url = new URL("https://www.example.com");
            String expected = "Success";

            //कनेक्शन बनाएं
            HttpURLConnection connection = (HttpURLConnection) url.openConnection();

            //रिस्पॉन्स को प्राप्त करें
            String response = connection.getResponseMessage();

            //रिस्पॉन्स की जांच करें
            if (response.equals(expected)) {
                System.out.println("Success!");
            } else {
                System.out.println("Failure. Expected response: " + expected + ". Actual response: " + response);
            }

        } catch (Exception e) {
            System.out.println("Error: " + e.getMessage());
        }
    }
}
```
नोट: यह उदाहरण स्ट्रिंग कंपेरिसन के लिए सुंदर