---
title:                "डीबग आउटपुट प्रिंट करना"
html_title:           "Java: डीबग आउटपुट प्रिंट करना"
simple_title:         "डीबग आउटपुट प्रिंट करना"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## क्यों

कभी-कभी हमारे द्वारा लिखे गए कोड में गलतियाँ होती हैं और हम इन्हें ठीक करने के लिए अपना समय खोव देते हैं। इसलिए, डिबग आउटपुट से अपने कोड की गलतियों को पकड़ना बहुत महत्वपूर्ण होता है।

## कैसे करें

```Java
System.out.println("यहाँ हम डिबग आउटपुट को कंसोल पर प्रिंट कर रहे हैं!");
```

ऊपर दिए गए कोड में, हम `System.out.println()` फंक्शन का उपयोग करके अपनी डिबग आउटपुट को कंसोल पर प्रिंट कर सकते हैं। इससे हम अपने कोड की गलतियों को पकड़ सकते हैं और सुधार सकते हैं।

```Java
int a = 5;
int b = 0;

System.out.println("a = " + a);
System.out.println("b = " + b);
System.out.println("a/b = " + a/b);
```

ऊपर दिए गए कोड में, हमने एक गणना की जो `b` को 0 से विभाजित करती है। इससे हमारा कोड गलत चल गया और कंसोल पर यह आउटपुट प्रिंट होगा:

```
a = 5
b = 0
Exception in thread "main" java.lang.ArithmeticException: / by zero
```

इससे हमें यह पता चल जाता है कि हमने सही तरीके से गणना नहीं की है और हम अपने कोड में सुधार कर सकते हैं।

## गहराई में जाएं

डिबग आउटपुट के लिए अन्य भी विकल्प हैं, जैसे `System.err.println()` जो गलतियों को रेड कलर में प्रिंट करता है और `System.out.print()` जो नयी लाइन शामिल नहीं करता है। हालांकि, अधिकांश लोग `System.out.println()` का उपयोग करते हैं क्योंकि यह सरल और आसान होता है।

## देखें भी

- [Debugging in Java - Tutorialspoint](https://www.tutorialspoint.com/java/java_debugging.htm)
- [Debugging Techniques in Java - GeeksforGeeks](https://www.geeksforgeeks.org/debug