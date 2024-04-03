---
date: 2024-01-26 04:12:15.014127-07:00
description: "\u65B9\u6CD5\uFF1A C++\u306B\u306F\u7D44\u307F\u8FBC\u307F\u306EREPL\u304C\
  \u4ED8\u5C5E\u3057\u3066\u3044\u307E\u305B\u3093\u304C\u3001Cling\u306E\u3088\u3046\
  \u306A\u30C4\u30FC\u30EB\u3067\u305D\u306E\u6A5F\u80FD\u3092\u63D0\u4F9B\u3059\u308B\
  \u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u3053\u3053\u3067\u306F\u3001Cling\u3092\
  \u4F7F\u7528\u3057\u30662\u3064\u306E\u6570\u5024\u306E\u5408\u8A08\u3092\u8A08\u7B97\
  \u3059\u308B\u65B9\u6CD5\u3092\u793A\u3057\u307E\u3059\uFF1A."
lastmod: '2024-03-13T22:44:42.555211-06:00'
model: gpt-4-0125-preview
summary: "C++\u306B\u306F\u7D44\u307F\u8FBC\u307F\u306EREPL\u304C\u4ED8\u5C5E\u3057\
  \u3066\u3044\u307E\u305B\u3093\u304C\u3001Cling\u306E\u3088\u3046\u306A\u30C4\u30FC\
  \u30EB\u3067\u305D\u306E\u6A5F\u80FD\u3092\u63D0\u4F9B\u3059\u308B\u3053\u3068\u304C\
  \u3067\u304D\u307E\u3059\u3002\u3053\u3053\u3067\u306F\u3001Cling\u3092\u4F7F\u7528\
  \u3057\u30662\u3064\u306E\u6570\u5024\u306E\u5408\u8A08\u3092\u8A08\u7B97\u3059\u308B\
  \u65B9\u6CD5\u3092\u793A\u3057\u307E\u3059\uFF1A."
title: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\uFF08REPL\uFF09\
  \u306E\u4F7F\u7528"
weight: 34
---

## 方法：
C++には組み込みのREPLが付属していませんが、Clingのようなツールでその機能を提供することができます。ここでは、Clingを使用して2つの数値の合計を計算する方法を示します：

```C++
#include <iostream>

int main() {
    int a = 5;
    int b = 7;
    std::cout << "The sum is: " << a + b << std::endl;
    return 0;
}

// 出力:
// 合計は：12
```

Clingを起動し、コードを1行ずつ入力して、各コマンドの後に出力を観察します。コンパイルなしで即座にフィードバックを受け取ります。

## 深掘り
REPLはPythonやLispのような言語に一般的であり、1960年代から存在しています。C++のようなコンパイル言語には概念が自然には合わないため、Clingのようなツールが存在します。これらはC++を即時に解釈します。代替手段にはオンラインコンパイラーや従来の方法でコンパイルされた小規模のテストプログラムが含まれます。ClingはLLVMとClangの上に構築されており、解釈方式でC++を使用するための橋渡しを提供します。

## 参照
- [Cling](https://root.cern/cling/): LLVMとClangライブラリの上に構築されたインタラクティブなC++インタプリタ。
- [Jupyter Notebooks](https://jupyter.org/): ノートブック環境内でインタラクティブシェルを提供し、xeus-clingカーネルを通じてC++をサポート。
- [LLVM](https://llvm.org/): コンパイラやツールチェーン技術のモジュール式で再利用可能なコレクション。Clingが基盤とするもの。
