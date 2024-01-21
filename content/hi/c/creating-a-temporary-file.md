---
title:                "अस्थायी फाइल बनाना"
date:                  2024-01-20T17:40:20.921721-07:00
model:                 gpt-4-1106-preview
simple_title:         "अस्थायी फाइल बनाना"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

अस्थायी फ़ाइल बनाना मतलब एक ऐसी फाइल जो डेटा को शॉर्ट-टर्म में स्टोर करे। प्रोग्रामर इसे इसलिए इस्तेमाल करते हैं क्योंकि इससे डेटा को अस्थायी रूप से रखने, सुरक्षित विनिमय करने और बूट खत्म होने पर अपने-आप मिटाने में सुविधा मिलती है।

## How to: (कैसे करें:)

C में अस्थायी फाइल बनाने के लिए आप `tmpfile()` स्टैंडर्ड लाइब्रेरी फंक्शन का इस्तेमाल कर सकते हैं।

```c
#include <stdio.h>

int main()
{
    // अस्थायी फाइल बनाना
    FILE *tmp = tmpfile();
    
    // वेरिफाई करना कि फ़ाइल सफलतापूर्वक बनी या नहीं
    if (tmp == NULL) {
        perror("tmpfile() failed");
        return 1;
    }
    
    // अस्थायी फ़ाइल में कुछ लिखना
    fputs("Hello, this is a temporary file!", tmp);
    
    // फ़ाइल को बंद करना
    fclose(tmp);
    
    // tmpfile() अपने आप फ़ाइल को मिटा देगा जब ऐप खत्म होगा
    return 0;
}
```

## Deep Dive (गहराई से जानकारी):

`tmpfile()` फंक्शन एक अस्थायी बाइनरी फाइल खोलता है जिसे read/write मोड में access किया जा सकता है। जब प्रोग्राम खत्म होता है, या तो नॉर्मल तरीके से या `exit` कॉल द्वारा, फाइल को स्वत: ही डिलीट कर दिया जाता है। अस्थायी फाइलों का इतिहास काफी पुराना है, और वे UNIX जैसे ऑपरेटिंग सिस्टम से शुरू हुईं।

अल्टरनेटिव्स में `mkstemp()`, `mkdtemp()`, या अस्थायी फाइलनेम बनाने के लिए `mktemp()` फंक्शंस इस्तेमाल हो सकते हैं, लेकिन इन्हें इस्तेमाल करते समय सावधानी रखने की जरूरत होती है क्योंकि ये फाइलनेम प्रेडिक्शन से सिक्योरिटी इश्यूज पैदा कर सकते हैं।

अस्थायी फाइलें आमतौर पर `/tmp` या `/var/tmp` डायरेक्ट्री में बनाई जाती हैं, और कई ऑपरेटिंग सिस्टम्स स्वयं इन्हें प्रबंधित करने के लिए सिस्टम कॉल प्रोवाइड करते हैं।

## See Also (इसे भी देखें):

- C Standard Library Documentation: tmpfile (https://en.cppreference.com/w/c/io/tmpfile)
- Linux man page for tmpfile (https://man7.org/linux/man-pages/man3/tmpfile.3.html)
- A discussion on creating temporary files securely in C (https://stackoverflow.com/questions/4328310/securely-creating-a-temporary-file-with-mkstemp)