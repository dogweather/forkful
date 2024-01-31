---
title:                "कमांड लाइन आर्गुमेंट्स पढ़ना"
date:                  2024-01-20T17:56:04.013527-07:00
model:                 gpt-4-1106-preview
simple_title:         "कमांड लाइन आर्गुमेंट्स पढ़ना"

category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
कमांड लाइन आर्ग्यूमेंट्स पढ़ने का मतलब है कि आपके प्रोग्राम को शुरू करते समय यूजर जो इनपुट देता है, उसे ग्रहण करना। यह जरूरी है ताकि आपका प्रोग्राम विभिन्न परिस्थितियों में चल सके और फ्लेक्सिबल रहे।

## How to: (कैसे करें:)
```C
#include <stdio.h>

int main(int argc, char *argv[]) {
    printf("आपने %d आर्ग्यूमेंट्स पास किए हैं:\n", argc - 1);
    for (int i = 1; i < argc; i++) {
        printf("%d: %s\n", i, argv[i]);
    }
    return 0;
}
```
सैंपल आउटपुट अगर आप इस प्रोग्राम को 'test' नामक एग्जिक्यूटेबल के साथ `./test hello world` रन करते हैं:
```
आपने 2 आर्ग्यूमेंट्स पास किए हैं:
1: hello
2: world
```

## Deep Dive (गहन विचार):
कमांड लाइन आर्ग्यूमेंट्स का उपयोग 1970 के दशक से UNIX आधारित सिस्टम्स में चल रहा है। `argc` बताता है कि कितने आर्ग्यूमेंट्स पास किए गए हैं, और `argv` आर्ग्यूमेंट्स के मूल्यों का एक एरे है। इसे अन्य भाषाओं में भी देखा जा सकता है। इसके अलावा, लाइब्रेरीज जैसे कि `getopt` और `argp` ज्यादा कॉम्प्लेक्स पार्सिंग फांक्शन्स प्रोवाइड करती हैं, जैसे कि ऑप्शन्स विथ फ्लैग्स।

## See Also (और देखें):
- GNU C Library Manual: https://www.gnu.org/software/libc/manual/
- `getopt` tutorial: https://www.gnu.org/software/libc/manual/html_node/Getopt.html
- POSIX standards for arguments: https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap12.html
