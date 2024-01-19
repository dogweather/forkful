---
title:                "एक पाठ फ़ाइल पढ़ना"
html_title:           "Bash: एक पाठ फ़ाइल पढ़ना"
simple_title:         "एक पाठ फ़ाइल पढ़ना"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

# What & Why?
टेक्स्ट फ़ाइल पढ़ना का मतलब होता है किसी टेक्स्ट फ़ाइल की सामग्री तक पहुँचना। प्रोग्रामर्स इसे इसलिए करते हैं क्योंकि यह उन्हें डाटा को निर्यात करने, गणना करने और मानित करने की अनुमति देता है। 

# How to:
यहां एक कोड का उदाहरण है जिसमें एक टेक्स्ट फ़ाइल पढ़ी जा रही है:

```C
#include <stdio.h>
 
int main() {
   char ch;
   FILE *fp;

   fp = fopen("test.txt", "r");

   while((ch = fgetc(fp)) != EOF)
      printf("%c", ch);

   fclose(fp);
   return 0;
}
```

यदि "test.txt" फ़ाइल में "Hello, World!" है, तो आउटपुट होगा:

```C
Hello, World!
```

# Deep Dive
तो अब हम इसे और अच्छी तरह से समझने का प्रयास करेंगे। 

1. ऐतिहासिक संदर्भ: यह कार्य करता है कि C प्रोग्रामिंग की मूल भाषा, B, में टेक्स्ट फ़ाइल पढ़ने की क्षमता नहीं थी। इसके बाद C में इसका समर्थन जोड़ा गया था। 

2. विकल्प: fgets(), fscanf() जैसे अन्य फ़ंक्शन भी हैं जिनका उपयोग टेक्स्ट फ़ाइल पढ़ने के लिए किया जा सकता है। 

3. कार्यान्वयन विवरण: fgetc() एक केंद्रीय क्लास जिसे फ़ाइल से एकल वर्ण पढ़ने के लिए इस्तेमाल किया जाता है। EOF एक विशेष मान है जिसे फ़ाइल के अंत का संकेत देने के लिए इस्तेमाल किया जाता है। 

# See Also
अधिक जानकारी के लिए निम्न लिंक्स देखें:
1. www.cplusplus.com/reference/cstdio/fgets/
2. en.wikipedia.org/wiki/C_file_input/output
3. www.geeksforgeeks.org/c-programming-file-handling/