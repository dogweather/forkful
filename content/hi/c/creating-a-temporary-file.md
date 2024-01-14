---
title:                "C: एक अस्थायी फ़ाइल बनाना"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्यों

अगर आप C प्रोग्रामिंग में नए हो और अपने code को सुरक्षित रखना चाहते हैं, तो temporary file बनाना आपके लिए बहुत महत्वपूर्ण हो सकता है। ये temporary file आपके code के execution के दौरान काम आ सकते हैं और आपके code को बचाने में मदद कर सकते हैं।

## कैसे

एक temporary file बनाने के लिए, आपको "tmpfile()" function का use करना होगा। नीचे एक संभावित coding example है:

```C
#include <stdio.h>

int main()
{
    FILE *temp_file;
    char temp_char;
    temp_file = tmpfile();           // temporary file बनाएं
    fprintf(temp_file, "Hello World!");     // temporary file में string लिखें
    rewind(temp_file);               // temporary file का शुरुआती position पर ले जाएं
    while((temp_char = fgetc(temp_file)) != EOF)      // temporary file को read करें
    {
        printf("%c", temp_char);          // character को stdout पर print करें
    }

    return 0;
}
```
आउटपुट:
Hello World!

## गहराई में जाएं

जब आप tmpfile() function को call करते हैं, तो एक unique file का pointer आपको return होता है जो कि temporary file को ले गया होता है। ये file एक temporary directory में create होता है और जब आप अपना program terminate करते हैं तो ये file automatically delete हो जाता है। आप अपने code को error handling के लिए temporary file में लिख सकते हैं और ये code के execution के बाद उन file को delete कर सकते हैं।

## देखिये

**एग्जाम्पल code के लिए:**
https://www.tutorialspoint.com/c_standard_library/c_function_tmpfile.htm

**और भी मजेदार C programming topics के लिए देखें:**
https://www.programmingsimplified.com/c-programming