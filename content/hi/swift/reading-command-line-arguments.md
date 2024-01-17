---
title:                "कम्प्यूटर प्रोग्रामिंग पर केवल सुझाव"
html_title:           "Swift: कम्प्यूटर प्रोग्रामिंग पर केवल सुझाव"
simple_title:         "कम्प्यूटर प्रोग्रामिंग पर केवल सुझाव"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

कमांड लाइन आर्ग्यूमेंट पढ़ने का मतलब है कि एक प्रोग्रामर अपने स्क्रिप्ट या आपातकालीन एप्प्स को आरगुमेंट के रूप में ऑपरेटिंग सिस्टम से प्राप्त कर सकता है। इसे पढ़ने के लिए बनाई गई भाषा हो सकती है।

## कैसे:

```Swift 
func main() {
    let arguments = CommandLine.arguments
    print(arguments)
}
```

जब आप यह कोड कंपाइल करोगे तो कमांड लाइन आर्ग्यूमेंट सूची प्रिंट हो जाएगी। यदि आपको अपने ऑपरेशन सिस्टम के आर्ग्युमेंट दिखने हैं तो आप उन्हें प्रिंट कर सकते हैं।

### आउटपुट:

```Swift
["./programName", "-o", "output.txt"]
```

यहां, हमारे कमांड लाइन में तीन आर्ग्यूमेंट्स हैं - `./programName`, `-o` और `output.txt`। आप यह देख सकते हैं कि आर्ग्युमेंट कैसे स्ट्रिंग या संख्याएं के रूप में हो सकते हैं।

### दूर दूर तक:

कमांड लाइन आर्ग्युमेंट्स की पहले से ही तारीख है। पहले इसे पढ़ने के लिए C और C++ का उपयोग किया जाता था। इसके अलावा, आप प्रोग्राम में आर्ग्युमेंट्स पेस करने के लिए एक विशेष फ़ंक्शन भी बना सकते हैं। इससे आप उन्हें प्रोजेक्ट के अन्य हिस्सों में उधेर नहीं भेजना पड़ेगा।

## उदाहरणहीन जानकारी:

- **NSProcessInfo**: यह एक Objective-C कक्रिप्टिंग क्लास है जो आपको कमांड लाइन आर्ग्युमेंट्स पढ़ने और जोड़ने की अनुमति देता है। 
- **ProcessInfo**: यह Swift में उसी प्रयोगशील रूप में काम करता है और NSProcessInfo की Objective-C थोड़ी सी हैै। 

## देखें:

- [Apple's Documentation](https://developer.apple.com/documentation/foundation/processinfo)
- [Basics of Command Line Arguments in Swift](https://www.hackingwithswift.com/example-code/system/how-to-read-command-line-arguments-using-commandlinetocuments)