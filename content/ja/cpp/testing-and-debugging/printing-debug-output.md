---
date: 2024-01-20 17:52:01.846804-07:00
description: "How to: (\u65B9\u6CD5) \u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u306F\u6614\
  \u304B\u3089\u3042\u308A\u307E\u3059\u3002`printf` \u3084 `cout` \u304C\u3088\u304F\
  \u4F7F\u7528\u3055\u308C\u307E\u3059\u3002IDE\u306E\u30C7\u30D0\u30C3\u30AC\u3068\
  \u6BD4\u3079\u308B\u3068\u3001\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u306F\u72B6\u6CC1\
  \u306B\u4F9D\u3089\u305A\u4F7F\u7528\u3067\u304D\u307E\u3059\u304C\u4E00\u6642\u7684\
  \u304B\u3064\u624B\u52D5\u306A\u65B9\u6CD5\u3067\u3059\u3002\u6761\u4EF6\u3092\u6307\
  \u5B9A\u3057\u3066\u7279\u5B9A\u306E\u72B6\u614B\u306E\u307F\u306B\u51FA\u529B\u3092\
  \u9650\u308B\u3053\u3068\u3082\u3067\u304D\u307E\u3059\u3002\u4F8B\u3048\u3070 `#ifdef\
  \ DEBUG`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.366498-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u306F\u6614\u304B\u3089\
  \u3042\u308A\u307E\u3059\u3002`printf` \u3084 `cout` \u304C\u3088\u304F\u4F7F\u7528\
  \u3055\u308C\u307E\u3059\u3002IDE\u306E\u30C7\u30D0\u30C3\u30AC\u3068\u6BD4\u3079\
  \u308B\u3068\u3001\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u306F\u72B6\u6CC1\u306B\u4F9D\
  \u3089\u305A\u4F7F\u7528\u3067\u304D\u307E\u3059\u304C\u4E00\u6642\u7684\u304B\u3064\
  \u624B\u52D5\u306A\u65B9\u6CD5\u3067\u3059\u3002\u6761\u4EF6\u3092\u6307\u5B9A\u3057\
  \u3066\u7279\u5B9A\u306E\u72B6\u614B\u306E\u307F\u306B\u51FA\u529B\u3092\u9650\u308B\
  \u3053\u3068\u3082\u3067\u304D\u307E\u3059\u3002\u4F8B\u3048\u3070 `#ifdef DEBUG`\
  \ \u30D7\u30EA\u30D7\u30ED\u30BB\u30C3\u30B5\u3092\u4F7F\u3044\u307E\u3059\u3002"
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
