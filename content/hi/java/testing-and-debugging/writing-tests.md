---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:40.395304-07:00
description: "\u0915\u0948\u0938\u0947: \u091C\u093E\u0935\u093E \u0921\u0947\u0935\
  \u0932\u092A\u0930\u094D\u0938 \u092E\u0941\u0916\u094D\u092F \u0930\u0942\u092A\
  \ \u0938\u0947 \u0926\u094B \u092A\u0930\u0940\u0915\u094D\u0937\u0923 \u092B\u094D\
  \u0930\u0947\u092E\u0935\u0930\u094D\u0915 \u0915\u093E \u0909\u092A\u092F\u094B\
  \u0917 \u0915\u0930\u0924\u0947 \u0939\u0948\u0902: JUnit \u0914\u0930 TestNG\u0964\
  \ \u092F\u0939\u093E\u0901, \u0939\u092E JUnit \u092A\u0930 \u0927\u094D\u092F\u093E\
  \u0928 \u0915\u0947\u0902\u0926\u094D\u0930\u093F\u0924 \u0915\u0930\u0947\u0902\
  \u0917\u0947, \u091C\u094B \u0907\u0938\u0915\u0940 \u0938\u093E\u0926\u0917\u0940\
  \ \u0914\u0930 \u0935\u094D\u092F\u093E\u092A\u0915\u2026"
lastmod: '2024-03-13T22:44:52.119732-06:00'
model: gpt-4-0125-preview
summary: "\u091C\u093E\u0935\u093E \u0921\u0947\u0935\u0932\u092A\u0930\u094D\u0938\
  \ \u092E\u0941\u0916\u094D\u092F \u0930\u0942\u092A \u0938\u0947 \u0926\u094B \u092A\
  \u0930\u0940\u0915\u094D\u0937\u0923 \u092B\u094D\u0930\u0947\u092E\u0935\u0930\u094D\
  \u0915 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0924\u0947 \u0939\
  \u0948\u0902."
title: "\u091F\u0947\u0938\u094D\u091F \u0932\u093F\u0916\u0928\u093E"
weight: 36
---

## कैसे:
जावा डेवलपर्स मुख्य रूप से दो परीक्षण फ्रेमवर्क का उपयोग करते हैं: JUnit और TestNG। यहाँ, हम JUnit पर ध्यान केंद्रित करेंगे, जो इसकी सादगी और व्यापक अपनाई गई वजह से परीक्षण लिखने के लिए अधिक लोकप्रिय विकल्प है।

### JUnit मूल बातें
अपनी Maven प्रोजेक्ट में JUnit का उपयोग करने के लिए, आपकी `pom.xml` में निम्नलिखित निर्भरता जोड़ें:

```xml
<dependency>
    <groupId>org.junit.jupiter</groupId>
    <artifactId>junit-jupiter</artifactId>
    <version>5.9.0</version>
    <scope>test</scope>
</dependency>
```

JUnit में एक मूलभूत परीक्षण इस प्रकार दिखता है:

```java
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class CalculatorTest {
    
    @Test
    public void testAdd() {
        Calculator calculator = new Calculator();
        assertEquals(5, calculator.add(2, 3), "2 + 3 का योग 5 होना चाहिए");
    }
}
```

इस परीक्षण को निष्पादित करने से या तो सफलता मिलेगी, जो इंगित करेगा कि `add` मेथड वांछित के अनुसार काम कर रही है, या विफलता होगी, जो एक त्रुटि संदेश दिखाएगा।

### Mockito के साथ Mocking
वास्तविक दुनिया के परिदृश्यों में, ऑब्जेक्ट अक्सर अन्य ऑब्जेक्ट्स पर निर्भर करते हैं। Mockito एक लोकप्रिय मॉकिंग फ्रेमवर्क है जो परीक्षण के लिए मॉक ऑब्जेक्ट्स बनाने में मदद करता है।

अपनी Maven प्रोजेक्ट में Mockito जोड़ें:

```xml
<dependency>
    <groupId>org.mockito</groupId>
    <artifactId>mockito-core</artifactId>
    <version>4.5.1</version>
    <scope>test</scope>
</dependency>
```

Mockito के साथ एक सरल उपयोग मामला:

```java
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

public class UserServiceTest {

    @Test
    public void testGetUsername() {
        // एक मॉक UserRepository बनाएं
        UserRepository mockRepository = mock(UserRepository.class);

        // मॉक ऑब्जेक्ट के लिए व्यवहार निर्धारित करें
        when(mockRepository.getUsername(1)).thenReturn("john_doe");

        UserService userService = new UserService(mockRepository);
        
        assertEquals("john_doe", userService.getUsername(1), "यूजर आईडी 1 को john_doe होना चाहिए");
    }
}
```

यह मॉक हमें एक वास्तविक `UserRepository` की आवश्यकता के बिना `UserService` का परीक्षण करने में मदद करता है, परीक्षण को `UserService` के भीतर के तर्क पर केंद्रित करता है।
