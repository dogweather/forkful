---
title:    "Java: एक टेक्स्ट फाइल पढ़ना"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

# क्यों

क्या आप जावा प्रोग्रामिंग में विशेषज्ञ बनना चाहते हैं? अगर हां, तो आपको टेक्स्ट फ़ाइल को पढ़ना सीखने में रुचि हो सकती है। इस आर्टिकल के माध्यम से, हम आपको बताएंगे कि आप कैसे जावा में एक टेक्स्ट फ़ाइल को पढ़ सकते हैं और इससे आपको कैसे लाभ हो सकता है।

# कैसे करें

टेक्स्ट फ़ाइल को जावा में पढ़ने के लिए, हम एक `FileReader` और `BufferedReader` का उपयोग कर सकते हैं। नीचे दिए गए कोड ब्लॉक में हम आपको एक उदाहरण देंगे जिसमें हम टेक्स्ट फ़ाइल से दिनांक को पढ़ रहे होते हैं और उसे स्क्रीन पर प्रिंट कर रहे होते हैं।

```Java
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

public class ReadTextFile {

    public static void main(String[] args) {
    
        File file = new File("example.txt");
        BufferedReader br = null;

        try {

            br = new BufferedReader(new FileReader(file));
            String date = "";
            
            while ((date = br.readLine()) != null) {
                System.out.println(date);
            }

        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            try {
                br.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }
}
```
यहां, हम `ReadTextFile` नाम के एक क्लास बना रहे हैं जो `example.txt` नाम की एक फ़ाइल से दिनांक को अलग-अलग लाइनों पर पढ़ता है। हम पहले `File` ऑब्जेक्ट बनाते हैं और फिर `BufferedReader` का उपयोग करके उस फ़ाइल को पढ़ते हैं। अंत में, हम `BufferedReader` को बंद कर देते हैं।

जब हम यह कोड अपने IDE में चलाते हैं, तो हमें निम्नलिखित आउटपुट मिलता है:

```
21/07/2020
22/07/2020
23/07/2020
24/07/2020
25/07/2020
```

इससे हम देखते हैं कि हमारी फ़ाइल में ये दिनांक