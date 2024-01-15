---
title:                "एक http अनुरोध भेजना"
html_title:           "Java: एक http अनुरोध भेजना"
simple_title:         "एक http अनुरोध भेजना"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्यों

जब आप इंटरनेट पर साइटों का इस्तेमाल करते हैं तो आपके द्वारा किसी दूसरे सर्वर पर जानकारी का अनुरोध किया जाता है। उदाहरण के लिए, जब आप वेब पेज को खोलते हैं तो आपके द्वारा सर्वर से आपके ब्राउजर में HTML दस्तावेज़ डाउनलोड होता है। हम इस प्रक्रिया को समझने के लिए "HTTP अनुरोध" कहते हैं।

## कैसे

कोडिंग उदाहरण के साथ डेस्कटॉप अनुरोध भेजना काफी सरल हो सकता है। नीचे दिए गए सामान्य स्टेप्स के माध्यम से आप एक "GET" अनुरोध भेज सकते हैं:

```Java
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.Scanner;

public class HttpExample {
    public static void main(String[] args) {
        try {
            // उदाहरण यूआरएल: google.com
            URL url = new URL("अपना URL यहां डालें");
            HttpURLConnection con = (HttpURLConnection) url.openConnection();
            con.setRequestMethod("GET");
            // सर्वर से अनुरोध करने के लिए:
            int responseCode = con.getResponseCode();
            System.out.println("अनुरोध का प्रतिक्रिया कोड:" + responseCode);
            // सर्वर से डेटा पाने के लिए:
            Scanner in = new Scanner(con.getInputStream());
            while (in.hasNextLine()) {
                String data = in.nextLine();
                System.out.println(data);
            }
            in.close();
        } catch (IOException e) {
            System.out.println("An error occurred: " + e);
        }
    }
}
```

आप इस कोड को कंपाइल और रन कर सकते हैं, जिससे आपको अपने द्वारा दिए गए यूआरएल से सर्वर से जुड़े आसानी से सामग्री को देख सकते हैं। आप में 'POST', 'PUT', और 'DELETE' भी लिख सकते हैं और उनके लिए भी संबंधित अनुरोध स्क्रिप्ट भेज सकते हैं।

## गहराई में जाएँ

"HTTP अनुरोध" एक TCP/IP बेस्ड कम्यूनिकेशन प्रोटोकॉल है। इसका उपयोग डाटा प्वाइंट सर्विसेज, एप