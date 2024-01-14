---
title:    "Java: Debug आउटपुट प्रिंटिंग"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## क्यों

जब हम अपनी जावा प्रोग्राम को लिखते हैं, हमें कभी-कभी प्रोग्राम के विभिन्न हिस्सों का डीबग आउटपुट कोन्सोल पर देखना होता है। यह हमारे कोड के दर्शकों की भूलों को पता लगाने या प्रोग्राम को ठीक करने में मदद कर सकता है।

## कैसे करें

डीबग आउटपुट प्रिंट करने के लिए, हम `System.out.println()` या `System.out.print()` फंक्शन का उपयोग कर सकते हैं। नीचे एक उदाहरण दिया गया है:

```Java
public class DebugExample {
    public static void main(String[] args) {
        int a = 5;
        int b = 10;
        System.out.println("Value of a: " + a);
        System.out.println("Value of b: " + b);
        System.out.println("Sum of a and b: " + (a + b));
    }
}
```

आउटपुट:

```
Value of a: 5
Value of b: 10
Sum of a and b: 15
```

यहां हमने `System.out.println()` का उपयोग करके अपनी कंप्यूटेशन के बीच मे स्थान लगाया है ताकि हम देख सकें कि कंपाइलर कैसे मैमोरी पर भूल के स्थान को आसानी से आउटपुट कर सकता है।

## गहराई में

लेकिन डीबग आउटपुट को सही से प्रिंट करने से पहले, हमें इस बात का ध्यान रखना चाहिए कि हमारा कोड क्या करने की कोशिश कर रहा है और हमें क्या आउटपुट चाहिए। हमेशा सुनिश्चित करें कि हम अपनी डीबग आउटपुट को प्रभावी और स्पष्ट बनाए रखते हैं ताकि कोड पढ़ने का काम आसान हो और दुगना कोसिस करने की जरूरत न हो।

## देखें भी

- [Java Debugging Techniques](https://www.geeksforgeeks.org/java-debugging-techniques/)
- [Debugging in Java](https://www.tutorialspoint.com/java/java_debugging.htm)
- [Debugging Tips and Tricks in Java](https://stackify.com/debugging-tips-and-tricks-in-java/)