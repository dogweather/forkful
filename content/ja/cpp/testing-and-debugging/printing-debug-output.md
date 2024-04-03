---
date: 2024-01-20 17:52:01.846804-07:00
description: "How to: (\u65B9\u6CD5) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.556472-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u3092\u8868\u793A\u3059\u308B"
weight: 33
---

## How to: (方法)
```C++
#include <iostream>

int main() {
    int total = 0;
    for (int i = 1; i <= 5; ++i) {
        total += i;
        std::cout << "i: " << i << ", total: " << total << '\n';
    }
}
```
出力:
```
i: 1, total: 1
i: 2, total: 3
i: 3, total: 6
i: 4, total: 10
i: 5, total: 15
```

## Deep Dive (掘り下げ)
デバッグ出力は昔からあります。`printf` や `cout` がよく使用されます。IDEのデバッガと比べると、デバッグ出力は状況に依らず使用できますが一時的かつ手動な方法です。条件を指定して特定の状態のみに出力を限ることもできます。例えば `#ifdef DEBUG` プリプロセッサを使います。

## See Also (関連情報)
- [cppreference.com](https://en.cppreference.com/w/cpp/io)
- [Stack Overflow: Debugging Techniques](https://stackoverflow.com/questions/495021/why-is-debugging-better-in-an-ide)
