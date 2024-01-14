---
title:    "Java: टेक्स्ट फाइल लिखना"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## आखिर क्यों:

क्या आपने कभी सोचा है कि कोडिंग क्यों इतनी महत्त्वपूर्ण है? यह आसान और सरल तरीका है अधिकतम प्रोग्रामिंग क्षमता खोलने का। यह आपको न केवल जानकारी सीमा में बंद रखता है, बल्कि आपको हर कार्य को पूरी तरह से कस्टमाइज करने का भी स्वतंत्रता देता है।

## कैसे करें:

कोडिंग के कई अभ्यास हो सकते हैं, लेकिन सामान्य रूप से एक पूर्ण प्रोग्राम के रूप में उन्हे संगठित करना बहुत अधिक अपरिचित हो सकता है। एक प्रैक्टिकल उदाहरण के रूप में, हम आपको आज आवश्यकता पड़ने पर टेक्स्ट फ़ाइल (text file) को कैसे लिखें और इसकी उपयोगिता के बारे में बताएंगे। 

````java
// टेक्स्ट फ़ाइल बनाना

import java.io.*;

public class TextFileWriter {

    public static void main(String[] args){
    // फ़ाइल का नाम और स्थान निर्दिष्ट करें
        String fileName = "myFile.txt";
        String filePath = "C:\\Users\\User\\Desktop";

        try{
            // टेक्स्ट फ़ाइल बनाने के लिए FileWriter का उपयोग करें
            FileWriter fileWriter = new FileWriter(filePath + "\\" + fileName);
            // टेक्स्ट लिखें
            fileWriter.write("यह एक उदाहरण पाठ फ़ाइल है।");
            // फ़ाइल बंद करें
            fileWriter.close();
            System.out.println("फ़ाइल सफलतापूर्वक लिखी गई!");

        } catch(IOException e){
            System.out.println("अशुभ, कुछ गलत हो गया।");
            e.printStackTrace();
        }
    }
}

````

उपरोक्त कोड स्निपेट में, हम `FileWriter` कक्षा के उपयोग से आसानी से एक टेक्स्ट फ़ाइल लिख सकते हैं। हम `write ()` फ़ंक्शन का उपयोग करके फ़ाइल में सामग्री लिख सकते हैं