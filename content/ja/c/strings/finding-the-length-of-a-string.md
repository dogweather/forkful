---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:56:39.420072-07:00
description: "\u65B9\u6CD5\uFF1A C\u8A00\u8A9E\u3067\u306F\u3001\u6A19\u6E96\u30E9\
  \u30A4\u30D6\u30E9\u30EA\u95A2\u6570`strlen()`\u304C\u4E00\u822C\u7684\u306B\u6587\
  \u5B57\u5217\u306E\u9577\u3055\u3092\u898B\u3064\u3051\u308B\u305F\u3081\u306B\u4F7F\
  \u308F\u308C\u307E\u3059\u3002\u3053\u3061\u3089\u304C\u7C21\u5358\u306A\u4F8B\u3067\
  \u3059\uFF1A."
lastmod: '2024-04-05T22:38:42.265802-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A C\u8A00\u8A9E\u3067\u306F\u3001\u6A19\u6E96\u30E9\u30A4\
  \u30D6\u30E9\u30EA\u95A2\u6570`strlen()`\u304C\u4E00\u822C\u7684\u306B\u6587\u5B57\
  \u5217\u306E\u9577\u3055\u3092\u898B\u3064\u3051\u308B\u305F\u3081\u306B\u4F7F\u308F\
  \u308C\u307E\u3059\u3002\u3053\u3061\u3089\u304C\u7C21\u5358\u306A\u4F8B\u3067\u3059\
  \uFF1A."
title: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6C42\u3081\u308B"
weight: 7
---

## 方法：
C言語では、標準ライブラリ関数`strlen()`が一般的に文字列の長さを見つけるために使われます。こちらが簡単な例です：

```c
#include <stdio.h>
#include <string.h>

int main() {
    char myString[] = "Hello, World!";
    size_t length = strlen(myString);
    
    printf("Length of '%s' is %zu.\n", myString, length);
    
    return 0;
}
```

**サンプル出力：**
```
Length of 'Hello, World!' is 13.
```

この例では、`strlen()`は文字列(`myString`)を入力として取り、ヌル終端を除いたその長さを返します。長さ変数に`size_t`を使用することが推奨されます。これは符号なし整数型であり、システム上で可能な最大オブジェクトのサイズを表現できる能力があるからです。

## 深掘り：
`strlen()`関数は、C言語の発祥以来C標準ライブラリの一部であります。内部的には、ヌル終端に達するまで文字列を横断しながらカウンタを増加させることで機能します。しかし、この単純さはパフォーマンス上の考慮事項を伴います：`strlen()`は実行時に文字を数えるため、例えば同じ文字列に対してループ内で繰り返し呼び出すことは非効率的です。

セキュリティの観点からは、`strlen()`や他のCの文字列処理関数はバッファオーバーランを本質的にチェックしません。したがって、脆弱性を避けるために注意深いプログラミングが必要です。長さを含む文字列型やデフォルトで安全なバッファ処理を使用する他言語の現代的な代替手段は、これらのリスクや非効率をいくつか解消します。

その制限にもかかわらず、低レベルのコードを扱う場合やパフォーマンスとメモリ管理が最優先される場合など、`strlen()`とC言語での手動文字列処理を理解することはプログラマーにとって重要です。また、他の言語のより高レベルの文字列抽象化の動作についての貴重な洞察も提供します。
