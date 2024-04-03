---
date: 2024-01-20 17:52:01.846804-07:00
description: "\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u306F\u30B3\u30FC\u30C9\u304C\u4F55\
  \u3092\u3057\u3066\u3044\u308B\u304B\u3092\u6559\u3048\u3066\u304F\u308C\u307E\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u305D\u308C\u306B\u3088\u3063\u3066\
  \u554F\u984C\u3092\u7D20\u65E9\u304F\u898B\u3064\u3051\u3001\u4FEE\u6B63\u3057\u307E\
  \u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.556472-06:00'
model: gpt-4-1106-preview
summary: "\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u306F\u30B3\u30FC\u30C9\u304C\u4F55\
  \u3092\u3057\u3066\u3044\u308B\u304B\u3092\u6559\u3048\u3066\u304F\u308C\u307E\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u305D\u308C\u306B\u3088\u3063\u3066\
  \u554F\u984C\u3092\u7D20\u65E9\u304F\u898B\u3064\u3051\u3001\u4FEE\u6B63\u3057\u307E\
  \u3059\u3002."
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
