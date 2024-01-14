---
title:    "C: डायरेक्टरी मौजूद है या नहीं जांचना"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## क्यों

अगर आप C programming सीख रहे हैं तो आपने शायद सुना होगा कि programming के दौरान directories की मौजूदगी की जांच करने का एक तरीका है। इस blog post में हम आपको बताएंगे कि आप directories के मौजूदगी को जांचने के कारण और तरीके जानते हैं।

## कैसे करें

```C
#include <stdio.h>
#include <stdlib.h>
#include <dirent.h>

int main() {
    char *directory_name = "mydirectory";
    DIR *dir;

    // opendir() फ़ंक्शन से directory को खोलें
    dir = opendir(directory_name);

    // यदि directory मौजूद है, तो सफलतापूर्वक folder का नाम प्रिंट करें
    if (dir) {
        printf("Directory '%s' मौजूद है।\n", directory_name);

        // opendir() फ़ंक्शन को बंद करें
        closedir(dir);
    }
    // यदि directory मौजूद नहीं है, तो त्रुटि मेसेज प्रिंट करें
    else {
        printf("Directory '%s' मौजूद नहीं है।\n", directory_name);
    }

    return 0;
}
```

### आउटपुट:
```
Directory 'mydirectory' मौजूद है।
```

## गहराई में

C programming में directories से संबंधित काम करने के लिए, हमे <dirent.h> header file का उपयोग करना पड़ता है। opendir() फ़ंक्शन को उस directory के नाम के साथ कॉल करने से directory को खोला जा सकता है। यदि directory मौजूद होता है, तो कोड के अनुसार हम सफलतापूर्वक प्रॉसेसिंग कर सकते हैं। opendir() फ़ंक्शन को हमेशा बंद करना जरूरी है नहीं तो हमारे प्रोग्राम में memory leaks हो सकता है।

## देखें भी

- [Dirent.h C Reference](https://www.geeksforgeeks.org/dirent-h-header-file-c/)
- [Introduction to File I/O in C](https://www.programiz.com/c-programming/c-file-input-output)
- [C Programming Tutorials in Hindi](https://www.youtube.com/playlist?list=PLvc3v2D3bKHTNdN3UNWpa1JTnXocQjWua)