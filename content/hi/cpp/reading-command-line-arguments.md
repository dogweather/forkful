---
title:                "C++: कम्प्यूटर प्रोग्रामिंग में कमांड लाइन आर्ग्यूमेंट्स पढ़ना"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## इसलिए
कॉमांड लाइन आर्ग्यूमेंट पढ़ने का कार्य शायद आपने कभी भी किया होगा, और अगर नहीं तो ये काम आपके लिए काफी दिलचस्प हो सकता है। इसके अलावा, ये आपको अपने कोड में प्रयोग करने के लिए एक उपयोगी तकनीक है।

## कैसे करें
यहाँ हम आपको बताएंगे कि कॉमांड लाइन आर्ग्यूमेंट्स को कैसे पढ़ा जाता है और इसका उपयोग कैसे किया जाता है। नीचे दिए गए कोड उदाहरण और साथ ही उनका आउटपुट शामिल है:

```C++
#include <iostream>
using namespace std;

int main(int argc, char* argv[])
{
  cout << "Total number of arguments: " << argc << endl;
  for (int i = 0; i < argc; i++)
  {
    cout << "Argument " << i + 1 << ": " << argv[i] << endl;
  }
  return 0;
}
```

आउटपुट:
```
Total number of arguments: 3
Argument 1: program.exe 
Argument 2: arg1
Argument 3: arg2
```

जैसा कि आप ऊपर दिए गए कोड से देख सकते हैं, `argc` और `argv` स्टैंडर्ड का उपयोग करके हम उपयोगकर्ता से दर्शाते हैं कि उन्होंने कितने और कौन से आर्ग्यूमेंट्स पास किए हैं। इसके बाद `for` लूप के माध्यम से हम सभी आर्ग्यूमेंट्स को प्रिंट करते हैं।

## गहराई में जाएँ
कॉमांड लाइन आर्ग्यूमेंट्स को पढ़ने के और उनका उपयोग करने के बारे में अधिक जानने के लिए आप `argc` और `argv` के अलावा `getopt()` और `boost::program_options` जैसे स्तरीय लाइब्रेरी का भी उपयोग कर सकते हैं। इन लाइब्रेरीज़ से आप आर्ग्यूमेंट्स को अधिक सुविधाजनक और विशेष कर सकते हैं