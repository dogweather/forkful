---
title:                "एक पाठ फ़ाइल पढ़ना"
html_title:           "Bash: एक पाठ फ़ाइल पढ़ना"
simple_title:         "एक पाठ फ़ाइल पढ़ना"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों (What & Why?)
एक टेक्स्ट फ़ाइल को पढ़ना मतलब उसकी सामग्री को कम्प्यूटर प्रोग्राम में लोड करना है। अक्सर, प्रोग्रामर्स इसे करते हैं ताकि वे इनपुट डाटा को प्राप्त कर सकें एवं इसे प्रसंस्करण कर सकें। 

## कैसे (How to):
जावा में, हम BufferedReader का उपयोग करके टेक्स्ट फ़ाइल पढ़ सकते हैं| निम्नलिखित उदाहरण में यह दिखाया गया है:

```Java
import java.io.*;

public class ReadFile {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("test.txt"));
            String line = null;
            while ((line = reader.readLine()) != null) {
                System.out.println(line);
            }
            reader.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```
यदि test.txt में "नमस्ते दुनिया" होता, तो आउटपुट होता:

```Shell
नमस्ते दुनिया
```

## और अधिक जानकारी (Deep Dive)
तो, फ़ाइल रीडिंग का इतिहास अपार है, क्योंकि यह सॉफ्टवेयर डेवलपमेंट का हिस्सा बनी हुई है |
वैकल्पिक रूप से, आप Scanner क्लास का उपयोग करके भी फ़ाइल पढ़ सकते हैं, जो कि एक शक्तिशाली और विस्तृत विकल्प है। 
"BufferedReader" आसानी से प्रबंधित करने वाली बड़ी फ़ाइलों के लिए प्रभावी तरीका है, क्योंकि यह एक पूरी लाइन को एक समय में पढ़ता है। 

## इससे भी देखें (See Also)
जवा द्वारा फ़ाइल पढ़ने तथा लिखने के बारे में और विस्तृत जानकारी के लिए, नीचे दिए गए स्रोतों पर विचार करें:

जवापोइंट (Javapoint): https://www.javapoint.com/java-file-io

टुटोरिअल्स प्वाइंट (Tutorials Point): https://www.tutorialspoint.com/java/java_files_io.htm

गीक्स फॉर गीक्स (Geeks for Geeks): https://www.geeksforgeeks.org/java-io-file-class-java/