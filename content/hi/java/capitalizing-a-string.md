---
title:    "Java: स्ट्रिंग में कैपिटल लगाना"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# क्यों

कोई भी भाषा में कोडिंग करते समय, अक्सर हमें स्ट्रिंग (string) के प्रारंभिक अक्षर को कैपिटलाइज (capitalize) करने की आवश्यकता होती है। आप यह करके अपने कोड को और अधिक सुंदर बना सकते हैं और उसे पढ़ने में आसानी होगी।

# कैसे करें

````Java
String str = "hello world";
String capitalizedStr = str.substring(0, 1).toUpperCase() + str.substring(1);
System.out.println(capitalizedStr);
````

यह कोड आपके द्वारा दिए गए अक्षरों को कैपिटलाइज करेगा और नीचे दिए गए आउटपुट को प्रिंट करेगा:

````
Hello world
````

यदि आपका स्ट्रिंग में कई अक्षर हों तो आगे के कोड उन सभी अक्षरों को कैपिटलाइज करेगा:

````Java
String str = "hello everyone";
String[] words = str.split(" ");
for (int i = 0; i < words.length; i++) {
  words[i] = words[i].substring(0, 1).toUpperCase() + words[i].substring(1);
}
String capitalizedStr = String.join(" ", words);
System.out.println(capitalizedStr);
````

यह कोड आपके स्ट्रिंग को शब्दों में विभाजित करेगा और फिर हर शब्द के प्रथम अक्षर को कैपिटलाइज करेगा। नीचे दिए गए आउटपुट को प्रिंट करेगा:

````
Hello Everyone
````

# गहराई में जाएं

कैपिटलाइजिंग एक स्ट्रिंग सिर्फ एक बेस केस (base case) है, जो कि स्ट्रिंग की पहली अक्षर को कैपिटलाइज करता है। लेकिन यदि आपको और अधिक ग्रामर लोजिक को समझना है तो आप अपने स्ट्रिंग का प्रथम अक्षर निर्दिष्ट वर्गों में छोड़ सकते हैं, जैसे कि स्पेस, डैश आदि। आप इसमें से अपने अपने कोड को और अधिक सुंदर बना सकते हैं और अपनी प्रोग्रामिंग कौशल को भी ब