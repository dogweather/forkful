---
title:                "डीबग आउटपुट प्रिंट करना"
html_title:           "Gleam: डीबग आउटपुट प्रिंट करना"
simple_title:         "डीबग आउटपुट प्रिंट करना"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## क्या और क्यों? / What & Why?

Debug आउटपुट प्रिंट करना का मतलब होता है कि प्रोग्राम के चलने के दौरान हम उसका बिहेवियर मोनिटर करते हैं। इसका मुख्य उद्देश्य होता है प्रोग्राम में होने वाले त्रुटियों का पता लगाना और इन्हें सुधारना।

## कैसे: / How to:

Java में debug मेंसज को प्रिंट करने के लिए हमें `System.out.println` (या उसका छोटा रूप, `System.out.print`) का उपयोग करना होता है। आईए एक उदाहरण देखते हैं:

```Java
public class Main {

  public static void main(String[] args) {
    for (int i = 0; i < 5; i++) {
      System.out.println("Debug: i का मूल्य " + i + " है।");
    }
  }
  
}
```
इस प्रोग्राम का आउटपुट होगा:

```
Debug: i का मूल्य 0 है।
Debug: i का मूल्य 1 है।
Debug: i का मूल्य 2 है।
Debug: i का मूल्य 3 है।
Debug: i का मूल्य 4 है।
```
## गहराई से जानकारी / Deep Dive:
सभी भाषाओं में debugging एक महत्वपूर्ण हिस्सा है। Debugging का उपयोग coordinated system को understand करने और troubleshoot करने के लिए हमेशा से किया जा रहा है। आप `System.out.print` के अलावा जैसे कि logging libraries का उपयोग कर सकते हैं। Logging libraries, जैसे कि Log4j या SLF4J, विस्तारपूर्वक तत्वों को handle करने का समर्थन करते हैं, जैसे कि log message का प्रारूप बदलना, और log messages को अलग-अलग file में लिखना।

## इसे भी देखें / See Also:
- [SLF4J Introduction (यहाँ क्लिक करें)](https://www.slf4j.org/manual.html)