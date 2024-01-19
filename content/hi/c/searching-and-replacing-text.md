---
title:                "पाठ की खोज और प्रतिस्थापन"
html_title:           "Bash: पाठ की खोज और प्रतिस्थापन"
simple_title:         "पाठ की खोज और प्रतिस्थापन"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

टेक्स्ट की खोज और बदलाव से हम किसी भी स्ट्रिंग में विशेष एलिमेंट्स का पता लगाने और उन्हें बदलने का काम करते हैं। प्रोग्रामर्स इसे करते हैं ताकि वे डाटा को उपयुक्त तरीके से प्रसंस्करण और मानिपुलेट कर सकें। 

## कैसे करें:

C में, `strstr` और `sprintf` इस्तेमाल होते हैं। हम पहले खोजते हैं, फिर बदलते हैं। 

```C
#include<stdio.h>
#include<string.h>

void searchAndReplace(char* source, char* search, char* replace){
    char buffer[1024] = { 0 };
    char *insertPoint = &buffer[0];
    char *tmp = source;
    char *foundHere;
    int searchLen = strlen(search);
    int replaceLen = strlen(replace);

    while( (foundHere = strstr(tmp, search)) ){
        memcpy(insertPoint, tmp, foundHere - tmp);
        insertPoint += foundHere - tmp;
        memcpy(insertPoint, replace, replaceLen);
        insertPoint += replaceLen;
        tmp = foundHere + searchLen;
    }

    strcpy(tmp, insertPoint);
    strcpy(source, buffer);
}

int main(){
    char testStr[100] = "Hello, world! I love the world!";
    printf("Before: %s\n", testStr);
    searchAndReplace(testStr, "world", "Earth");
    printf("After: %s\n", testStr);
    return 0;
}
```

सैंपल आउटपुट:

    Before: Hello, world! I love the world!
    After: Hello, Earth! I love the Earth!

## गहरी बातचीत

टेक्स्ट सर्च और रिप्लेस की कामना प्रोग्रामिंग की शुरुआत से रही है। `strstr` और `sprintf` C स्टैंडर्ड लाइब्रेरी का हिस्सा हैं। इन समस्याओं के लिए अन्य विकल्प भी हैं। जैसे की `gnu` लाइब्रेस द्वारा प्रदान किया गया `strcasestr` और `strfry`। इसे लागू करने वाला कोड `buffer` का उपयोग करके काम करता है, जिसे हम 'insertPoint' के माध्यम से index करते हैं ताकि हमें जोड़ना हो, वहां जोड़ सकें।

## यह भी देखें

1. [`strstr` डॉक्युमेंटेशन - Cplusplus.com](http://www.cplusplus.com/reference/cstring/strstr/)
2. [`sprintf` डॉक्युमेंटेशन - Cplusplus.com](http://www.cplusplus.com/reference/cstdio/sprintf/)
3. [GNU लाइब्रेस डॉक्युमेंटेशन](https://www.gnu.org/software/libc/manual/)