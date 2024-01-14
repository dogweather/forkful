---
title:    "C: एक अस्थायी फ़ाइल बनाना"
keywords: ["C"]
---

{{< edit_this_page >}}

## क्यों

एक अस्थायी फाइल बनाने का काम इसलिए किया जाता है क्योंकि यह प्रोग्रामर को उस समय इस्तेमाल होने वाले डेटा को सेव करने और प्रोसेस के बाद उसे हटाने के लिए किया जाता है।

## कैसे करें

```C
#include <stdio.h>
#include <stdlib.h>

int main()
{
  // Creating a temporary file
  FILE *temp = tmpfile();

  // Writing data to the file
  fprintf(temp, "This is a temporary file created by C programming.");

  // Reading the data from the file
  rewind(temp);
  char data[100];
  while (fgets(data, 100, temp)){
    printf("%s", data);
  }

  // Closing the file
  fclose(temp);
  
  return 0;
}
```

आउटपुट: This is a temporary file created by C programming.

## गहराई में जाएं

अस्थायी फाइल क्रिएशन साधन अन्य फाइल सिस्टम क्रिएशन से थोड़ी अलग है। इसमें एक फाइल डेस्क्रिप्टर तीन अलग होते हैं - STDIN, STDOUT, और STDERR। इसके अलावा, इन अस्थायी फाइल सिस्टम में आप केवल इन प्रोसेस के दौरान उपयोग कर सकते हैं और प्रोसेस को समाप्त होने के बाद हमेशा के लिए हटा दिया जाएगा।

## देखें भी

- [अस्थायी फाइल बनाने के बारे में और अधिक जानें](https://www.geeksforgeeks.org/tmpfile-fopen-for-temporary-files-in-c-c/)
- [C programming में फाइल ऑपरेशन की जानकारी प्राप्त करें](https://www.programiz.com/c-programming/c-file-input-output)
- [C programming का GitHub रिपोजिटरी देखें](https://github.com/search?q=C+programming)