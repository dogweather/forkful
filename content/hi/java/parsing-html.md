---
title:                "HTML पार्स करना"
html_title:           "C++: HTML पार्स करना"
simple_title:         "HTML पार्स करना"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/parsing-html.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
HTML पार्सिंग हमारे समाहारक को HTML कोड में लिखूई जानकारी को समझने की क्षमता देता है। प्रोग्रामर्स इसे प्रयोग करते हैं ताकि वे HTML पेज से डाटा को संग्रहित और प्रयोग कर सकें।

##  कैसे करें:
सबसे पहले JSoup लाइब्ररी उपयोग करके HTML पार्स करने का उदाहरण देखें:

```Java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;

public class Main {
  public static void main(String[] args) {
    String html = "<html><head><title>मेरा पहला वेब पेज</title></head>"
                   + "<body><p>यहाँ पाठ होता है।</p></body></html>";
                   
    Document doc = Jsoup.parse(html);
    System.out.println(doc.title());
    System.out.println(doc.body().text());
  }
}
```
आउटपुट:

```
मेरा पहला वेब पेज
यहाँ पाठ होता है।
```
JSoup लाइब्ररी ने HTML को पार्स करके शीर्षक और बॉडी में लिखी जानकारी को प्रिंट किया है।

## गहराई की ओर:
HTML पार्सिंग की आवश्यकता 1990 के दशक में उत्पन्न हुई जब WWW आरंभ हुआ। इसके विकल्पों में HTMLUnit, JSoup, и HtmlCleaner, आदि शामिल हैं। यदि आप संगठनात्मक डाटा के साथ काम कर रहे हों, तो आप JSON या XML पार्सिंग भी विचार कर सकते हैं। पार्सिंग के अंतर्गत हमारे पार्सर कोड ने इनपुट HTML को डरबेन (ट्री) संरचना में परिवर्तित किया, तथा फिर हम उस डाटा का उपयोग कर सके।

## अधिक जानने के लिए:
* [JSoup लाइब्ररी](https://jsoup.org/)
* [HTMLUnit](http://htmlunit.sourceforge.net/)
* [HTMLCleaner](https://htmlcleaner.sourceforge.io/)
* [XML पार्सिंग](https://www.tutorialspoint.com/java_xml/java_dom_parse_document.htm)
* [JSON पार्सिंग](https://www.javatpoint.com/json-tutorial)

इन स्रोतों पर जाकर आपको HTML पार्सिंग की अधिक जटिलताओं को समझने में मदद मिलेगी।