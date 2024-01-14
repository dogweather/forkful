---
title:                "Java: डीबग आउटपुट प्रिंट करना"
simple_title:         "डीबग आउटपुट प्रिंट करना"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## क्यों

वैसे तो, जावा में डीबग आउटपुट प्रिंट करने का मतलब है कि हम अपने आवेदन को समझने में मदद करने के लिए उसमें क्या हो रहा है या कॉड के किसी हिस्से को पता करने के लिए चलाएंगे। इससे हमारे कोड में त्रुटियां एक आसान तरीके से खोजी जा सकती हैं और हमें इन्हें ठीक करने में मदद मिल सकती है।

## कैसे करें

यदि हम जानना चाहते हैं कि हमारा कोड क्या कर रहा है और कहां त्रुटि हो सकती है, तो हमे ```System.out.println()``` को प्रिंट स्टेटमेंट के साथ उपयोग करना चाहिए। यह आउटपुट को कंसोल पर प्रिंट करेगा।

चलिए देखें कि यह कोड किस तरह काम करता है:

```Java
public class DebugOutput {
    public static void main(String[] args) {
        int num1 = 10;
        int num2 = 20;
        System.out.println("नंबर 1 = " + num1);
        System.out.println("नंबर 2 = " + num2);

        int sum = sum(num1, num2);
        System.out.println("दोनों नंबरों का योग = " + sum);
    }

    public static int sum(int num1, int num2) {
        int result = num1 + num2;
        System.out.println("संख्या 1 + संख्या 2 के परिणाम = " + result);
        return result;
    }
}
```

आउटपुट:

```
नंबर 1 = 10
नंबर 2 = 20
संख्या 1 + संख्या 2 के परिणाम = 30
दोनों नंबरों का योग = 30
```

जैसा कि आप देख सकते हैं, हमने ```System.out.println()``` का उपयोग करके संख्याओं को प्रिंट किया और दोनों संख्याओं का योग भी प्रिंट किया। हमने अपने कोड में डीबग आउटपुट उपयोग करके चलाया है और यह हमें संख्याओं में त्रुटियों का प्रत्याश दिखाता है।

## गहराई से जानें

जब हम