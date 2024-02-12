---
title:                "टेस्ट लिखना"
date:                  2024-02-03T19:31:40.395304-07:00
model:                 gpt-4-0125-preview
simple_title:         "टेस्ट लिखना"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
जावा में परीक्षण लिखना विभिन्न परिस्थितियों के तहत आपके कोड का वांछित व्यवहार सुनिश्चित करने के बारे में है। प्रोग्रामर बग्स से बचने, परिवर्तनों के बाद फ़ंक्शनैलिटी सही बनी रहने की सुनिश्चितता और अच्छे सॉफ्टवेयर डिजाइन सिद्धांतों को बढ़ावा देने के लिए परीक्षण लिखते हैं।

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
