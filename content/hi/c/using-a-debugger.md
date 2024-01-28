---
title:                "डीबगर का उपयोग"
date:                  2024-01-26T03:49:02.602868-07:00
model:                 gpt-4-0125-preview
simple_title:         "डीबगर का उपयोग"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/using-a-debugger.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
डीबगर एक उपकरण है जो आपको अपने सी कोड का निरीक्षण करने देता है, जब वह चल रहा होता है, चरण दर चरण, ताकि बग्स का पता लगा सके। प्रोग्रामर्स डीबगर्स का उपयोग अपने कोड के व्यवहार को समझने, समस्याओं को ठीक करने और अनुमान लगाने के खेल के बिना प्रदर्शन को अनुकूलित करने के लिए करते हैं।

## कैसे:
मान लीजिए आप एक सरल सी प्रोग्राम के साथ काम कर रहे हैं जो एक संख्या का गुणांक (factorial) निकालता है, लेकिन वहाँ एक गड़बड़ी है। `gdb` (GNU Debugger) जैसे डीबगर का उपयोग करने के लिए, पहले `-g` फ्लैग के साथ कंपाइल करें ताकि डीबग जानकारी शामिल हो:

```c
// compile with: gcc factorial.c -o factorial -g
#include <stdio.h>

long factorial(int n) {
    if (n < 0) return 0; // नकारात्मक इनपुट के लिए एक सरल जांच
    long result = 1;
    while (n > 1)
        result *= n--;
    return result;
}

int main() {
    int number = 5;
    long result = factorial(number);
    printf("The factorial of %d is %ld\n", number, result);
    return 0;
}
```

फिर इसे gdb में चलाएँ:

```shell
$ gdb ./factorial
```

`factorial` फ़ंक्शन पर एक ब्रेकप्वाइंट सेट करें और प्रोग्राम को चलाएँ:

```gdb
(gdb) break factorial
(gdb) run
```

जब यह ब्रेकप्वाइंट पर पहुंचता है, `next` या `n` का उपयोग करके प्रत्येक पंक्ति के माध्यम से चरणबद्ध करें और `print` या `p` के साथ चर की जांच करें:

```gdb
(gdb) next
(gdb) print result
$1 = 1
```

नमूना आउटपुट वास्तविक समय मूल्य और कार्यक्रम कार्यान्वयन प्रवाह प्रदान करेगा।

## गहराई में
डीबगर 1960 के दशक से रहे हैं, साधारण मॉनिटर्स से लेकर जटिल, GUI-आधारित अनुप्रयोगों तक विकसित होते हुए। परिपक्व डीबगर्स विकसित होने से पहले पुराने स्कूल की प्रिंट-आधारित डीबगिंग आम थी। `gdb` के विकल्पों में `lldb`, `dbx`, या Visual Studio या CLion जैसे IDE-इंटीग्रेटेड डीबगर्स शामिल हैं। 

डीबगर्स से निपटते समय, कार्यान्वयन भिन्न होता है—कुछ रनटाइम त्रुटियों को पकड़ सकते हैं, मेमोरी का परीक्षण कर सकते हैं, या यहां तक कि कार्यक्रम का क्रियान्वयन उलट सकते हैं। `gdb` चल रही प्रक्रियाओं से जुड़ सकता है, जो पहले से चल रहे सॉफ्टवेयर के डीबगिंग के लिए, और लाइव सिस्टम बग्स को ठीक करने के लिए एक वरदान है।

## देखें भी
- GNU डीबगर (GDB): https://www.gnu.org/software/gdb/documentation/
- GDB के साथ डीबगिंग: https://sourceware.org/gdb/current/onlinedocs/gdb
- LLDB डीबगर: https://lldb.llvm.org/use/tutorial.html
- C में डीबगिंग तकनीकें: http://www.cprogramming.com/debugging/debugging.html
