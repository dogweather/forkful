---
title:                "C: क्या एक निर्देशिका मौजूद है या नहीं देखना।"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## क्यों

क्या आपने कभी सोचा है कि आपको प्रोग्रामिंग में फ़ाइल या डायरेक्ट्री की उपस्थिति को कैसे जांचना चाहिए? एक नौकरी से मुक़ाबले करते हुए, हमारी हर दिनलिखित कार्यों में फ़ाइलें या डायरेक्ट्री काफी महत्वपूर्ण हो सकती हैं। इसलिए इस ब्लॉग पोस्ट में, हम आपको बताएंगे कि यह क्यों महत्वपूर्ण है और आपको इसे कैसे करना चाहिए।

## कैसे

आप अपने कोड में निम्नलिखित तरीके से दिए गए कोड ब्लॉक का उपयोग करके फाइल या डायरेक्ट्री की उपस्थिति को जांच सकते हैं। 

```C
#include <stdio.h>
#include <stdbool.h>
#include <sys/stat.h>

// Function to check if a directory exists
bool checkDirectory(const char *path) {
    struct stat stats;
    
    // Use stat() function to get information about the file or directory
    if (stat(path, &stats) != 0) {
        return false;
    }
    
    // Check if it is a directory
    if (S_ISDIR(stats.st_mode)) {
        return true;
    }
    
    return false;
}

int main() {
    // Path of the directory to be checked
    char *path = "/path/to/directory";
    
    // Call the checkDirectory() function
    if (checkDirectory(path)) {
        printf("Directory exists!");
    } else {
        printf("Directory does not exist.");
    }
    
    return 0;
}
```

आप ऊपर दिए गए कोड ब्लॉक में दिए गए कोमेंट्स का भी उपयोग कर सकते हैं ताकि आपको समझ में आसानी हो। इस कोड का आउटपुट निम्न रूप में होगा:

```
Directory exists!
```

इस तरह से आप फ़ाइल या डायरेक्ट्री की उपस्थिति को आसानी से जांच सकते हैं।

## डीप डाइव

इस ब्लॉग पोस्ट में हमने `stat()` फ़ंक्शन का उपयोग किया है जो कि लीनक्स और यूनिक्स के आधार पर बने ऑपरेटिंग सिस्टम में समर्थित है। इसके अलावा आप `access()` फ़ंक्शन का भी उपयो