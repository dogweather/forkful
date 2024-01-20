---
title:                "एक वेब पेज डाउनलोड करना"
html_title:           "Kotlin: एक वेब पेज डाउनलोड करना"
simple_title:         "एक वेब पेज डाउनलोड करना"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

वेब पेज डाउनलोड करना मतलब वेब सर्वर से वेब पेज को अपने कंप्यूटर में स्थानांतरित करना। कोडर इसे डेटा एनालिसिस, वेब स्क्रेपिंग, और ऑफलाइन उपयोग के लिए करते हैं।

## कैसे करें:

```Java
import java.io.*;
import java.net.URL;

public class Main {
   public static void main(String[] args) throws Exception {
      URL url = new URL("http://www.example.com");
      BufferedReader reader = new BufferedReader(new InputStreamReader(url.openStream()));
      String line;

      while((line = reader.readLine()) != null) {
         System.out.println(line);
      }
      reader.close();
   }
}
```
इसे जब रन किया जाता है, तो यह example.com का एचटीएमएल स्रोत कोड दिखाता है:

```Java
<!doctype html>
<html>
<head>
   <title>Example Domain</title>
   ...
```
## गहरी डाइव

वेब पेज डाउनलोड करने की क्षमता का इस्तेमाल करके, वेब का विस्तार और क्षमताएं बढ़ती गई हैं। वास्तविक दुनिया में, कई बार प्रोग्रामर्स लाइब्रेरीज , जैसे कि JSoup (Java Site Scraper) का उपयोग करते हैं, जो एचटीएमएल पैर्सिंग, CSS-like selectors, और अन्य सुविधा समर्थन करता है।

## संबंधित देखें:

- [Java के साथ HTML पृष्ठों को पर्स करने के लिए JSoup गाइड](https://jsoup.org/cookbook/extracting-data/selector-syntax)
- [Apache HttpClient लाइब्रेरी का उपयोग कर के वेब पेज डाउनलोड](https://mkyong.com/java/apache-httpclient-examples/)