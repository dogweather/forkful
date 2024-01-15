---
title:                "वेब पन्ना डाउनलोड करना"
html_title:           "Java: वेब पन्ना डाउनलोड करना"
simple_title:         "वेब पन्ना डाउनलोड करना"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्यों

वेब पेज डाउनलोड करने का काम अगर आपको दिखता है तो आपको संभवतः यह जानने की तलाश होगी कि यह क्यों किया जाता है। यह आमतौर पर डेटा या सामग्री के सम्पादन या वेबआई के माध्यम से डेटा को अपलोड करने के लिए विशिष्ट प्रतिक्रियाओं का हिस्सा होता है।

## कैसे करें

वेब पेज को डाउनलोड करने के लिए, आपको HttpURLConnection वर्ग का उपयोग करना होगा। नीचे दिए गए कोड उदाहरण में, हम सर्वर से सामग्री को प्राप्त करने के लिए इस्तेमाल करते हैं। साथ ही, हम सामग्री को आमतौर पर एक फ़ाइल में सुंचित करते हैं।

```Java
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;

public class WebPageDownloader {
    public static void main(String[] args) {
        String url = "https://www.example.com";
        String filePath = "sample.html";
        
        try {
            // URL बनाने और कनेक्शन खोलने की प्रक्रिया
            URL webpageUrl = new URL(url);
            HttpURLConnection connection = (HttpURLConnection) webpageUrl.openConnection();
            
            // सर्वर से सामग्री प्राप्त करने के लिए स्ट्रीम खोलना
            InputStream inputStream = connection.getInputStream();
            
            // सामग्री को एक फ़ाइल में सुंचित करना
            File webpageFile = new File(filePath);
            webpageFile.createNewFile();
            
            FileOutputStream fileOutputStream = new FileOutputStream(webpageFile);
            int readByte = inputStream.read();
            while (readByte != -1) {
                fileOutputStream.write(readByte);
                readByte = inputStream.read();
            }
            
            // स्ट्रीम को बंद करना
            inputStream.close();
            fileOutputStream.close();
            
            System.out.println("वेब पेज सफलतापूर्वक डाउनलोड किया गया है!");
        } catch (Exception e) {
            System.out.println("एक समस्या उद्भव हुई है: " + e.getMessage());
        }
    }
}
```

उपरोक्त कोड ने स्तंभित URL से सामग्री को डाउनलोड करके "sample.html" नाम की फ़ाइल में सुंचित किया है। आप किसी भी वेब पेज के लिए इसका उपयोग कर सकते हैं।

## गहराई में जाएं