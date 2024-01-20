---
title:                "कमांड लाइन तर्कों को पढ़ना"
html_title:           "Kotlin: कमांड लाइन तर्कों को पढ़ना"
simple_title:         "कमांड लाइन तर्कों को पढ़ना"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

##  क्या & क्यों?

कमांड लाइन आर्गुमेंट्स पढ़ना मतलब होता है प्रोग्राम को चलाते समय डाटा का इनपुट देना। इसे प्रोग्रामर्स उन्हें अधिक संविधाजनक और लचीला बनाने के लिए करते हैं।


## कैसे करें:

```C
#include <stdio.h>

int main(int argc, char *argv[]) {
// argc आर्गुमेंट की गिनती है और argv उन आर्गुमेंट्स का array है|
   printf("\nआपने %d arguments डाले हैं: ", argc);

   for(int i = 0; i < argc; i++) {
      printf("\nargv[%d]: %s", i, argv[i]);
   }

   return 0;
}
```

आपकी आउटपुट इस प्रकार होगी:

```C
आपने 2 arguments डाले हैं:

argv[0]: /path/to/program
argv[1]: argument
```

## गहराई में: 

**ऐतिहासिक प्रकर्ण:**  कमांड लाइन आर्गुमेंट्स का उपयोग लाइन-ओरिएंटेड कंप्यूटर सिस्टम के दिनों से किया जा रहा है। 
**विकल्प:**  अन्य तरीकों में मैन्युअल इनपुट, फाइल इनपुट, जीयूआई जैसे विकल्प शामिल हैं। 
**कार्यान्वयन विवरण**: `argc` का उपयोग कमांड लाइन पर प्रदान की गई आर्गुमेंट्स की संख्या ज्ञात करने के लिए किया जाता है| `argv[]` array में प्रदान की गई सभी आर्गुमेंट्स संग्रहित की जाती हैं।