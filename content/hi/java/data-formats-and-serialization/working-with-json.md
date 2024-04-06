---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:52.081619-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: \u0906\u0907\u090F\
  \ \u0905\u092A\u0928\u0940 \u092C\u093E\u091C\u0941\u0913\u0902 \u0915\u094B \u091A\
  \u0922\u093C\u093E\u090F\u0902 \u0914\u0930 \u091C\u093E\u0935\u093E \u092E\u0947\
  \u0902 JSON \u0915\u0947 \u0938\u093E\u0925 \u0915\u094B\u0921\u093F\u0902\u0917\
  \ \u0936\u0941\u0930\u0942 \u0915\u0930\u0947\u0902\u0964 \u0938\u092C\u0938\u0947\
  \ \u092A\u0939\u0932\u0947, \u0906\u092A\u0915\u094B \u090F\u0915 JSON \u092A\u094D\
  \u0930\u094B\u0938\u0947\u0938\u093F\u0902\u0917 \u0932\u093E\u0907\u092C\u094D\u0930\
  \u0947\u0930\u0940 \u091C\u0948\u0938\u0947 `Jackson` \u092F\u093E `Google\u2026"
lastmod: '2024-04-05T21:53:54.159314-06:00'
model: gpt-4-0125-preview
summary: "\u0906\u0907\u090F \u0905\u092A\u0928\u0940 \u092C\u093E\u091C\u0941\u0913\
  \u0902 \u0915\u094B \u091A\u0922\u093C\u093E\u090F\u0902 \u0914\u0930 \u091C\u093E\
  \u0935\u093E \u092E\u0947\u0902 JSON \u0915\u0947 \u0938\u093E\u0925 \u0915\u094B\
  \u0921\u093F\u0902\u0917 \u0936\u0941\u0930\u0942 \u0915\u0930\u0947\u0902\u0964\
  \ \u0938\u092C\u0938\u0947 \u092A\u0939\u0932\u0947, \u0906\u092A\u0915\u094B \u090F\
  \u0915 JSON \u092A\u094D\u0930\u094B\u0938\u0947\u0938\u093F\u0902\u0917 \u0932\u093E\
  \u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u091C\u0948\u0938\u0947 `Jackson` \u092F\
  \u093E `Google Gson` \u0915\u0940 \u0906\u0935\u0936\u094D\u092F\u0915\u0924\u093E\
  \ \u0939\u094B\u0917\u0940\u0964 \u092F\u0939\u093E\u0901 \u0939\u092E `Jackson`\
  \ \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0947\u0902\u0917\u0947\
  , \u0907\u0938\u0932\u093F\u090F \u0905\u092A\u0928\u0947 `pom.xml` \u092E\u0947\
  \u0902 \u092F\u0939 \u0928\u093F\u0930\u094D\u092D\u0930\u0924\u093E \u091C\u094B\
  \u0921\u093C\u0947\u0902."
title: "JSON \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
weight: 38
---

## कैसे करें:
आइए अपनी बाजुओं को चढ़ाएं और जावा में JSON के साथ कोडिंग शुरू करें।

सबसे पहले, आपको एक JSON प्रोसेसिंग लाइब्रेरी जैसे `Jackson` या `Google Gson` की आवश्यकता होगी। यहाँ हम `Jackson` का उपयोग करेंगे, इसलिए अपने `pom.xml` में यह निर्भरता जोड़ें:

```xml
<dependency>
    <groupId>com.fasterxml.jackson.core</groupId>
    <artifactId>jackson-databind</artifactId>
    <version>2.13.1</version>
</dependency>
```

अब, चलिए एक साधारण जावा ऑब्जेक्ट को JSON में सीरियलाइज़ (लिखना) करें:

```java
import com.fasterxml.jackson.databind.ObjectMapper;

public class JsonExample {
    public static void main(String[] args) {
        try {
            ObjectMapper mapper = new ObjectMapper();
            Person person = new Person("Alex", 30);
            String json = mapper.writeValueAsString(person);
            System.out.println(json);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

class Person {
    public String name;
    public int age;

    public Person(String name, int age) {
        this.name = name;
        this.age = age;
    }
}
```

आउटपुट होगा:

```json
{"name":"Alex","age":30}
```

अब, एक जावा ऑब्जेक्ट में JSON को वापस डिसीरियलाइज़ (पढ़ना) के लिए:

```java
import com.fasterxml.jackson.databind.ObjectMapper;

public class JsonExample {
    public static void main(String[] args) {
        String json = "{\"name\":\"Alex\",\"age\":30}";
        try {
            ObjectMapper mapper = new ObjectMapper();
            Person person = mapper.readValue(json, Person.class);
            System.out.println(person.name + " is " + person.age + " years old.");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

आउटपुट होगा:

```
Alex is 30 years old.
```

## गहराई से जानकारी
JSON की सादगी और प्रभावकारिता ने इसे वेब पर डेटा एक्सचेंज के लिए डी फैक्टो मानक बना दिया है, जिससे XML को इसके सिंहासन से हटा दिया गया। 2000 के शुरुआती दशक में पेश किया गया, JSON को जावास्क्रिप्ट से प्राप्त किया गया था लेकिन अब यह ज्यादातर भाषाओं में समर्थित है।

JSON के विकल्पों में XML शामिल है, जो अधिक वर्बोज़ है, और Protocol Buffers या MessagePack जैसे बाइनरी प्रारूप शामिल हैं, जो कम मानव-पठनीय हैं लेकिन आकार और गति में अधिक कुशल हैं। प्रत्येक के अपने उपयोग के मामले होते हैं; चयन आपकी विशिष्ट डेटा आवश्यकताओं और संदर्भ पर निर्भर करता है।

जावा में, `Jackson` और `Gson` के अलावा, हमारे पास JSON को संभालने के लिए `JsonB` और `org.json` जैसी अन्य लाइब्रेरियाँ हैं। Jackson स्ट्रीम-आधारित प्रोसेसिंग प्रदान करता है और इसे गति के लिए जाना जाता है, जबकि Gson इसके उपयोग में आसानी के लिए प्रसिद्ध है। JsonB जकार्ता EE का हिस्सा है, जो एक अधिक मानकीकृत दृष्टिकोण प्रदान करता है।

JSON को लागू करते समय, अपने अपवादों को उचित रूप से संभालना याद रखें - आपका कोड बुरे इनपुट के खिलाफ मजबूत होना चाहिए। साथ ही, स्वचालित डेटा बाइंडिंग के सुरक्षा निहितार्थों पर विचार करें - हमेशा अपने इनपुटों का सत्यापन करें!

## देखें
- [Jackson प्रोजेक्ट](https://github.com/FasterXML/jackson)
- [Gson प्रोजेक्ट](https://github.com/google/gson)
- [JSON विनिर्देश](https://www.json.org/json-en.html)
- [JsonB विनिर्देश](https://jakarta.ee/specifications/jsonb/)
