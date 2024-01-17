---
title:                "कंप्यूटर प्रोग्रामिंग पर एक लेख को “टेक्स्ट फ़ाइल लिखना” अनुवादित करें।"
html_title:           "Kotlin: कंप्यूटर प्रोग्रामिंग पर एक लेख को “टेक्स्ट फ़ाइल लिखना” अनुवादित करें।"
simple_title:         "कंप्यूटर प्रोग्रामिंग पर एक लेख को “टेक्स्ट फ़ाइल लिखना” अनुवादित करें।"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## यह क्या है और क्यों?
Writing a text file क्या होता है के बारे में दो तीन सेंटेंसेस और यह करने का उद्देश्य क्या है, प्रोग्रामर्स इसे क्यों करते हैं।

Text file के लिए लिखना Programming में एक आम काम है, जो डेटा और सूचनाओं को संरचित और सामान्य रूप से दृश्यमान बनाता है। प्रोग्रामर्स इसका उपयोग डेटा एक्सेस करने, लेखन अथवा रीड करने और अन्य ऐसे कामों के लिए करते हैं।

## कैसे करें?
```kotlin
// एक नया text file create करना
val file = File("myFile.txt")

// text file में data write करना
file.writeText("This is some text.")

// text file से data read करना
val data = file.readText() // data = "This is some text."

// text file में अतिरिक्त data append करना
file.appendText("And this is some more text.")
```

## गहराई में जाएं

Text file को write करना  बहुत काम सिंग क्‍‍यूरेशिन और संभावना का विकल्प है। एक उल्लेखनीय लाभ है कि text files platform-independent होते हैं, जोकि किसी भी ऑपरेटिंग सिस्टम पर काम करते हैं। अन्य विकल्प में XML, CSV फाइल और डेटाबेस भी शामिल हैं।

Text file के लिए कोडिंग करते समय, कुछ बातों का ध्यान रखना जरूरी होता है: सबसे पहले, एक text file variable बनाना जरूरी है जो file handling में इस्तेमाल होगा। दूसरा, कोडिंग करते समय यह जानना जरूरी है कि कैसे data write, read, append करें। इसके लिए File class के functions का इस्तेमाल कर सकते हैं। इन सब के अलावा, फाइल और data की security भी बहुत महत्वपूर्ण है। उन अनुरोधों को पूरा करने के लिए, आपको यह सुनिश्चित करना होगा कि आप data को सुरक्षित स्थान पर स्टोर करते हैं।

## संबंधित स्रोत
- [Kotlin Official Documentation on File Handling](https://kotlinlang.org/docs/reference/file-input-output.html)
- [JavaTpoint Tutorial on Writing to a File in Kotlin](https://www.javatpoint.com/kotlin-write-to-a-file)
- [Tutorialspoint Example of Reading a File using Kotlin](https://www.tutorialspoint.com/kotlin_programming/kotlin_reading_file.htm)