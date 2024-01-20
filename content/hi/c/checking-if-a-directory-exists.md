---
title:                "डायरेक्टरी मौजूद है या नहीं, कैसे जाँचें"
html_title:           "C: डायरेक्टरी मौजूद है या नहीं, कैसे जाँचें"
simple_title:         "डायरेक्टरी मौजूद है या नहीं, कैसे जाँचें"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

डायरेक्टरी की मौजूदगी की जांच यह सत्यापित करने की प्रक्रिया है कि एक कंप्यूटर की फाइल सिस्टम में निर्दिष्ट डायरेक्टरी मौजूद है या नहीं। प्रोग्रामर्स इसे करते हैं ताकि उन्हें ऐसा कोई ऑपरेशन न करें जो किसी अदृश्य डायरेक्टरी पर निर्भर करता हो।

## कैसे करें:

निम्नलिखित कोड इसे करने का एक तरीका दिखाता है:

```C
#include <sys/stat.h>
#include <stdbool.h>

bool is_dir_exists(const char *path)
{
   struct stat stats;
   stat(path, &stats);
   return S_ISDIR(stats.st_mode);
}

int main()
{
   const char* path = "/path/to/directory";
   if (is_dir_exists(path))
   {
      printf("%s directory exists\n", path);
   }
   else
   {
      printf("%s directory does not exist\n", path);
   }
   return 0;
}
```

इस कोड का आउटपुट इस प्रकार होगा:

```
/path/to/directory directory exists
```
या

```
/path/to/directory directory does not exist
```

## गहराई में जानकारी:

डायरेक्टरी की मौजूदगी की जांच का एक और तरीका है POSIX `access()` फ़ंक्शन का उपयोग करना। `access()` फ़ंक्शन एक आवेदनकर्ता की पहचान के आधार पर एक फ़ाइल या डायरेक्टरी के पाठ्य पहुंच की जांच करता है। हालाँकि, इसे कड़ाई से उपयोग करें क्योंकि यह स्थिति को अपने "जांचने और उपयोग करने" के बीच में बदलने की संभावना को नहीं रोकता है।

## आगे देखें:

1. [GNU C Library: File System Interface](https://www.gnu.org/software/libc/manual/html_node/File-System-Interface.html)
2. [Stack Overflow: How to check if a directory exists in C?](https://stackoverflow.com/questions/12510874/how-can-i-check-if-a-directory-exists)
3. [Cplusplus.com: <sys/stat.h>](http://www.cplusplus.com/reference/clibrary/csys/stat/)