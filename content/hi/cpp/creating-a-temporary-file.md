---
title:                "एक अस्थायी फाइल बनाना"
html_title:           "C++: एक अस्थायी फाइल बनाना"
simple_title:         "एक अस्थायी फाइल बनाना"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्यों

कभी-कभी अपने कोड में आपको एक अस्थायी फ़ाइल की आवश्यकता होती है जो कि आपको अपने काम को प्रभावी ढंग से पूरा करने में मदद करती है। इस लेख में हम आपको बताएंगे कि अस्थायी फ़ाइल क्या है और कैसे आप C++ में उसे बना सकते हैं।

## कैसे

जब अपने कोड रन किया जाता है, तो उसे रेममेंबर और स्टॉर करने के लिए एक अस्थायी फ़ाइल की आवश्यकता होती है। इसे बनाने के लिए हम यहां ```tmpfile()``` फ़ंक्शन का उपयोग करेंगे।

```C++
#include <stdio.h>

// Creating a temporary file using "tmpfile()"

int main(){

  FILE * tmpFile;
  int value = 10;

  tmpFile = tmpfile();  // Creating the temporary file

  if(tmpFile == NULL){  // Checking if file was created successfully
    printf("Error in creating the temporary file");
    return 0;
  }

  fprintf(tmpFile, "This file was created using tmpfile() function!"); // Writing to the temporary file
  fprintf(tmpFile, "\nValue = %d", value);
  fclose(tmpFile);  // Closing the temporary file

  return 0;
}
```

उपरोक्त कोड को रन करने पर, आपको एक अस्थायी फ़ाइल बनाई गई होगी जिसमें ```This file was created using tmpfile() function!``` और ```Value = 10``` के साथ कुछ पाठ लिखा हुआ होगा।

## गहराई में

अस्थायी फ़ाइलें सिस्टम के temp फ़ोल्डर में स्थानांतरित होती हैं जो कि दौरान कोड के अंत में स्वचालित रूप से हट जाती हैं। यह एक आसान, शीर्षक और text स्टाइल फ़ाइल होती हैं जिन्हें कृत्रिम रूप में इस्तेमाल किया जाता है।

## देखें भी

- [C++ File Handling](https://www.geeksforgeeks.org/c-file-handling/)
- [Temporary File in C++](https://www.geeksforgeeks.org/tmpfile-function-in-c-c/)
- [Quick explanation of tmpfile() in C](https://stackoverflow.com/questions/3135484/quick-explanation-of-tmpfile-in-c)