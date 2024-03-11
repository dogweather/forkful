---
date: 2024-01-26 04:33:49.378142-07:00
description: "Java \u0915\u0947 \u0938\u093E\u0925 XML \u0915\u0947 \u0938\u093E\u0925\
  \ \u0915\u093E\u092E \u0915\u0930\u0928\u093E \u0907\u0938\u0915\u0947 \u0935\u093F\
  \u0936\u094D\u0932\u0947\u0937\u0923 (parsing), \u092A\u094D\u0930\u0936\u094D\u0928\
  \ (querying), \u0914\u0930 XML \u0926\u0938\u094D\u0924\u093E\u0935\u0947\u091C\u094B\
  \u0902 \u0915\u0947 \u0938\u0902\u0936\u094B\u0927\u0928 (manipulating) \u0915\u094B\
  \ \u0936\u093E\u092E\u093F\u0932 \u0915\u0930\u0924\u093E \u0939\u0948\u0964 \u092A\
  \u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0910\u0938\u093E\u2026"
lastmod: '2024-03-11T00:14:26.054237-06:00'
model: gpt-4-0125-preview
summary: "Java \u0915\u0947 \u0938\u093E\u0925 XML \u0915\u0947 \u0938\u093E\u0925\
  \ \u0915\u093E\u092E \u0915\u0930\u0928\u093E \u0907\u0938\u0915\u0947 \u0935\u093F\
  \u0936\u094D\u0932\u0947\u0937\u0923 (parsing), \u092A\u094D\u0930\u0936\u094D\u0928\
  \ (querying), \u0914\u0930 XML \u0926\u0938\u094D\u0924\u093E\u0935\u0947\u091C\u094B\
  \u0902 \u0915\u0947 \u0938\u0902\u0936\u094B\u0927\u0928 (manipulating) \u0915\u094B\
  \ \u0936\u093E\u092E\u093F\u0932 \u0915\u0930\u0924\u093E \u0939\u0948\u0964 \u092A\
  \u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0910\u0938\u093E\u2026"
title: "XML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?
Java के साथ XML के साथ काम करना इसके विश्लेषण (parsing), प्रश्न (querying), और XML दस्तावेजों के संशोधन (manipulating) को शामिल करता है। प्रोग्रामर ऐसा डेटा इंटरचेंज, कॉन्फ़िगरेशन मैनेजमेंट और कई पुराने सिस्टम और APIs जो XML का उपयोग करते हैं, के लिए करते हैं।

## कैसे:
Java, DOM (Document Object Model), SAX (Simple API for XML), और StAX (Streaming API for XML) जैसे APIs प्रदान करता है ताकि XML के साथ काम किया जा सके। यहाँ एक DOM उदाहरण है जो XML फाइल का विश्लेषण करता है:

```java
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class XmlParser {
    public static void main(String[] args) {
        try {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            DocumentBuilder builder = factory.newDocumentBuilder();
            Document doc = builder.parse("data.xml");
            
            doc.getDocumentElement().normalize();
            NodeList nodeList = doc.getElementsByTagName("employee");
            
            for (int i = 0; i < nodeList.getLength(); i++) {
                Element element = (Element) nodeList.item(i);
                System.out.println("Name: " + element.getElementsByTagName("name").item(0).getTextContent());
                System.out.println("Age: " + element.getElementsByTagName("age").item(0).getTextContent());
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

मान लें `data.xml` ऐसा दिखता है:

```xml
<employees>
    <employee>
        <name>Jane Doe</name>
        <age>30</age>
    </employee>
    <employee>
        <name>John Doe</name>
        <age>40</age>
    </employee>
</employees>
```

आउटपुट होगा:

```
Name: Jane Doe
Age: 30
Name: John Doe
Age: 40
```

## गहराई में:
XML 90 के दशक के अंत से मौजूद है, जो विभिन्न सिस्टमों में डेटा के आदान-प्रदान के लिए एक संरचित और लचीला तरीका प्रदान करता है। हालांकि JSON ने इसकी सरल सिंटैक्स और JavaScript के साथ तंग एकीकरण के कारण नई वेब APIs के लिए अधिक लोकप्रियता प्राप्त कर ली है, XML उद्यम वातावरणों, SOAP-आधारित वेब सेवाओं और Microsoft Office के लिए Office Open XML जैसे दस्तावेज़ मानकों में व्यापक रूप से उपयोग किया जाता है।

Java में XML का विश्लेषण करते समय, DOM API छोटे दस्तावेज़ों के लिए शानदार है: यह ट्री-आधारित है और मेमोरी में XML संरचना तक पूरी पहुँच प्रदान करता है। हालांकि, बड़ी फ़ाइलों के लिए, यह स्मृति-गहन हो सकता है। SAX और StAX अधिक स्मृति-मित्रतापूर्ण हैं, क्योंकि वे क्रमशः इवेंट-चालित और स्ट्रीम-आधारित हैं, लेकिन XML संरचनाओं के नेविगेट करने के लिए वे कम सुविधाजनक हो सकते हैं।

XML बनाने या संशोधित करने के लिए, Java javax.xml.transform और javax.xml.bind (JAXB) पैकेज भी प्रदान करता है। JAXB जावा SE के संस्करण 10 तक Java SE का हिस्सा था, उसके बाद, जावा SE से जावा EE मॉड्यूल्स को हटाने के कारण यह एक अलग पुस्तकालय बन गया। यह XML से जावा ऑब्जेक्ट्स को सीरियलाइज़ करने और इसके विपरीत के लिए एक एनोटेशन-चालित तरीका है।

## देखें भी
Java में XML के साथ काम करने पर और जानकारी के लिए इन संबंधित स्रोतों को देखें:
- [Java API for XML Processing (JAXP)](https://docs.oracle.com/javase/8/docs/technotes/guides/xml/jaxp/index.html)
- [Java Architecture for XML Binding (JAXB)](https://javaee.github.io/jaxb-v2/)
- [Oracle की Java में XML के लिए गाइड](https://docs.oracle.com/javase/tutorial/jaxp/index.html)
- [W3C XML प्रौद्योगिकी](https://www.w3.org/standards/xml/)
- [Stack Overflow: 'java' और 'xml' टैग्स के साथ टैग किए गए प्रश्न](https://stackoverflow.com/questions/tagged/java+xml)
