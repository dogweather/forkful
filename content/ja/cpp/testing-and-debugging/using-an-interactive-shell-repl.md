---
date: 2024-01-26 04:12:15.014127-07:00
description: "REPL\uFF08Read-Eval-Print-Loop; \u8AAD\u307F\u53D6\u308A-\u8A55\u4FA1\
  -\u51FA\u529B-\u30EB\u30FC\u30D7\uFF09\u306F\u3001\u30B7\u30F3\u30D7\u30EB\u3067\
  \u5BFE\u8A71\u7684\u306A\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u74B0\u5883\u3067\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30EA\u30A2\u30EB\u30BF\
  \u30A4\u30E0\u3067\u8A00\u8A9E\u3092\u8A66\u3059\u3001\u7C21\u5358\u306A\u30BF\u30B9\
  \u30AF\u3092\u884C\u3046\u3001\u307E\u305F\u306F\u30D5\u30EB\u30D6\u30ED\u30FC\u30F3\
  \u306E\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u3092\u4F5C\u6210\u3059\u308B\
  \u30AA\u30FC\u30D0\u30FC\u30D8\u30C3\u30C9\u306A\u3057\u306B\u65B0\u3057\u3044\u6982\
  \u5FF5\u3092\u7406\u89E3\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u4F7F\u7528\
  \u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.555211-06:00'
model: gpt-4-0125-preview
summary: "REPL\uFF08Read-Eval-Print-Loop; \u8AAD\u307F\u53D6\u308A-\u8A55\u4FA1-\u51FA\
  \u529B-\u30EB\u30FC\u30D7\uFF09\u306F\u3001\u30B7\u30F3\u30D7\u30EB\u3067\u5BFE\u8A71\
  \u7684\u306A\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u74B0\u5883\u3067\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30EA\u30A2\u30EB\u30BF\u30A4\u30E0\
  \u3067\u8A00\u8A9E\u3092\u8A66\u3059\u3001\u7C21\u5358\u306A\u30BF\u30B9\u30AF\u3092\
  \u884C\u3046\u3001\u307E\u305F\u306F\u30D5\u30EB\u30D6\u30ED\u30FC\u30F3\u306E\u30A2\
  \u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u3092\u4F5C\u6210\u3059\u308B\u30AA\u30FC\
  \u30D0\u30FC\u30D8\u30C3\u30C9\u306A\u3057\u306B\u65B0\u3057\u3044\u6982\u5FF5\u3092\
  \u7406\u89E3\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u4F7F\u7528\u3057\u307E\
  \u3059\u3002"
title: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\uFF08REPL\uFF09\
  \u306E\u4F7F\u7528"
---

{{< edit_this_page >}}

## 何となぜ？
REPL（Read-Eval-Print-Loop; 読み取り-評価-出力-ループ）は、シンプルで対話的なプログラミング環境です。プログラマーは、リアルタイムで言語を試す、簡単なタスクを行う、またはフルブローンのアプリケーションを作成するオーバーヘッドなしに新しい概念を理解するためにこれを使用します。

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
