---
title:                "डीबग आउटपुट प्रिंट करना"
date:                  2024-01-20T17:52:00.895987-07:00
model:                 gpt-4-1106-preview
simple_title:         "डीबग आउटपुट प्रिंट करना"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

Debug output प्रोग्राम में मौजूद bugs या समस्याओं को समझने के लिए console पर messages print करने का तरीका है। Programmers इसे अपने कोड को better understand करने, issues को identify करने और fix करने के लिए use करते हैं।

## How to: (कैसे करें:)

C में debug messages के लिए, `printf` function का इस्तेमाल होता है। ये एक basic example है:

```c
#include <stdio.h>

int main() {
    int testValue = 5;
    printf("Debug: testValue is %d\n", testValue);

    // यहां आप अपना कोड लिखें

    return 0;
}
```

Sample output होगा:

```
Debug: testValue is 5
```

## Deep Dive (गहराई में):

पहले, debugging के लिए physical lights और switches का उपयोग हुआ करता था। बाद में, debugging messages की जरूरत पड़ी। आज, `printf` सबसे सामान्य तरीका है, लेकिन इसके alternatives भी हैं जैसे `fprintf` जो एक खास file में output को redirect करती है। Moreover, बड़े projects में, integrated debuggers जैसे gdb का इस्तेमाल होता है। Implementation detail में जाएं, तो `printf` library function सिस्टम के buffer में data लिखती है और फिर उसे console पर display करती है।

## See Also (और भी देखें):

- GNU Debugger (GDB) के बारे में अधिक जानकारी : [GDB Documentation](https://www.gnu.org/software/gdb/documentation/)
- `printf` और दूसरे input/output functions के लिए : [C Standard Library - IO functions](http://www.cplusplus.com/reference/cstdio/)
- C Programming Language, 2nd Edition by Brian W. Kernighan and Dennis M. Ritchie – इस किताब में deep technical details हैं।