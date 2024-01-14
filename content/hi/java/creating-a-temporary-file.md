---
title:    "Java: कार्यकारी फ़ाइल बनाना"
keywords: ["Java"]
---

{{< edit_this_page >}}

हम् क्यों: अस्थायी फ़ाइल बनाने में संलग्नता किसी को लोग केंद्रित करना चाहिए।

"अस्थायी फ़ाइलों को उसकी पहली भुगतान करने में आवश्यकता होती है जो वे बनाने के बाद हटा दिया जाते हैं। इसका प्रयोजन सीमित समय के आवश्यकताओं जैसे कि डाटा बांक, अप्लीकेशन टेस्टिंग और सुरक्षा उद्देश्यों के लिए हो सकता है।"

कैसे करें:

```Java
import java.io.File;
import java.io.IOException;

public class TemporaryFile {

    public static void main(String[] args) {
        try {
            // Create a temp file with prefix and suffix
            File tempFile = File.createTempFile("data", ".txt");

            // Output the absolute path of the temp file
            System.out.println("Temp file created: " + tempFile.getAbsolutePath());

            // Write data to the temp file
            // ...

            // Delete the temp file after its purpose
            tempFile.delete();

            System.out.println("Temp file deleted successfully!");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

आउटपुट:

```
Temp file created: C:\Users\Username\AppData\Local\Temp\data8603296756616120871.txt
Temp file deleted successfully!
```

गहराई में जाएँ:

अस्थायी फ़ाइल बनाने के लिए `File.createTempFile()` मेथड का उपयोग किया जाता है जो दो पैरामीटर लेता है - एक प्रीफिक्स और दूसरा सफिक्स। इन पैरामीटरों के साथ ऊपर दिए गए उदाहरण में `data` और `.txt` का उपयोग किया गया है। अस्थायी फ़ाइल में डेटा लिखने के बाद, हम `delete()` मेथड का उपयोग करके उसे हटा सकते हैं।

See Also:
- [https://docs.oracle.com/javase/7/docs/api/java/io/File.html#createTempFile(java.lang.String,%20java.lang.String)](https://docs.oracle.com/javase/7/docs/api/java/io/File.html#createTempFile(java.lang.String,%20java.lang.String))
- [https://www.journaldev.com/861/java-temporary-file-example](https://www.journaldev.com/861/java-temporary-file-example)
- [https://www.geeksforgeeks.org/file-createfile-method-in-java-with-examples/](https://www.geeksforgeeks.org/file-createfile-method-in-java-with-examples/)