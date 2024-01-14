---
title:    "C++: Please Note: पढ़ना translates to reading in Hindi, but may not be the best translation for this context.कम्प्यूटर प्रोग्रामिंग पर संबंधित लेख का नाम है कमांड लाइन आर्ग्यूमेंट्स पढ़ना"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## क्यों

क्या आपने कभी सोचा है कि आप अपने कंप्यूटर को आपके आवश्यकताओं के अनुसार काम करने के लिए इनपुट का उपयोग कर सकते हैं? यदि हां, तो आपको कमांड लाइन आर्ग्यूमेंट्स के बारे में जानने की आवश्यकता है। इस प्रकार से, आपको कंप्यूटर पर स्वयं कहानियां लिखने के लिए प्रोग्रामिंग की जरूरत नहीं होगी।

## कैसे

```C++
#include <iostream>

int main(int argc, char* argv[])
{
  // कमांड लाइन आर्ग्यूमेंट्स के प्रिंट करें
  for (int i = 0; i < argc; ++i)
    std::cout << "आपने भेजा हुआ आर्ग्यूमेंट: " << argv[i] << std::endl;
    
  return 0;
}
```

```
आपने इस प्रोग्राम को इस प्रकार चलाया होगा:
$ ./command_line_arguments.exe hello world
```

```
आपने भेजा हुआ आर्ग्यूमेंट: ./command_line_arguments.exe
आपने भेजा हुआ आर्ग्यूमेंट: hello
आपने भेजा हुआ आर्ग्यूमेंट: world
```

## गहराई में नज़र डालें

कमांड लाइन आर्ग्यूमेंट्स को पढ़ना अत्यंत सुविधाजनक होने के साथ-साथ बहुत उपयोगी है। आप इनके माध्यम से अपने प्रोग्राम को और भी उपयोगी बना सकते हैं। आगे बढ़े और अपनी पहली कमांड लाइन आर्ग्यूमेंट्स वाली प्रोग्राम लिखें!

## देखें भी

[इंटरनेशनलीज़ कमांड लाइन आर्ग्यूमेंट्स बाजार](https://www.callicoder.com/java-command-line-arguments/)

[कंपाइलिंग और रनिंग मदव्यूस साथ-साथ देखें](https://www.learn-c.org/en/Compiling)

[आर्ग्यूमेंट्स गहराई में जाओ](https://www.cplusplus.com/forum/beginner/44105/)

[कंपाइलिंग सं