---
title:                "यह जांचना कि डायरेक्टरी मौजूद है या नहीं"
date:                  2024-01-19
simple_title:         "यह जांचना कि डायरेक्टरी मौजूद है या नहीं"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
जांचना कि एक डायरेक्टरी मौजूद है या नहीं वह प्रक्रिया है जिससे हम C प्रोग्राम में फाइल सिस्टम का पता लगाते हैं। यह जानकारी का अधिग्रहण करना आवश्यक है ताकि प्रोग्राम कोई त्रुटि न करे जब वह डायरेक्टरी तक पहुंचने या उसमें लिखने की कोशिश करे।

## How to: (कैसे करें:)
```C
#include <stdio.h>
#include <sys/stat.h>

int directory_exists(const char *path) {
    struct stat statbuf;
    if (stat(path, &statbuf) != 0) {
        return 0; // डायरेक्टरी मौजूद नहीं है
    }
    return S_ISDIR(statbuf.st_mode); // यह जांचता है कि यह डायरेक्टरी है या नहीं
}

int main() {
    const char *path = "/path/to/directory";
    if (directory_exists(path)) {
        printf("डायरेक्टरी '%s' मौजूद है.\n", path);
    } else {
        printf("डायरेक्टरी '%s' मौजूद नहीं है.\n", path);
    }
    return 0;
}
```
सैंपल आउटपुट:

`डायरेक्टरी '/path/to/directory' मौजूद है.`

या

`डायरेक्टरी '/path/to/directory' मौजूद नहीं है.`

## Deep Dive (गहराई में जानकारी)
पारंपरिक C भाषा में `stat()` फ़ंक्शन का इस्तेमाल करके हम फ़ाइल सिस्टम में ऑब्जेक्ट्स की जानकारी ले सकते हैं। `stat` संरचना में `st_mode` फील्ड होता है जो फाइल-प्रकार और परमिशन्स के बारे में सूचना देता है। `S_ISDIR` मैक्रो यह जांचता है कि क्या यह डायरेक्टरी है। 

इसके विकल्प के रूप में POSIX स्टैंडर्ड `opendir()` और `readdir()` फ़ंक्शन्स भी हैं, लेकिन `stat()` अधिक सीधा और आम तरीका है।

पीछे की तकनीकी प्रक्रियाओं में, सिस्टम OS केर्नेल फाइल सिस्टम टेबल्स की जांच करता है और आवश्यक जानकारी लौटाता है। 

## See Also (संबंधित सूत्रों)
- `man 2 stat` – `stat()` फ़ंक्शन के लिए UNIX मैनुअल पेज।
- `man opendir` – डायरेक्टरी स्ट्रीम के लिए POSIX आधारित फ़ंक्शन को देखने के लिए।
- [GNU C Library: File Attributes](https://www.gnu.org/software/libc/manual/html_node/File-Attributes.html) – GNU C Library में फ़ाइल विशेषताओं के बारे में अधिक जानकारी।
