---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:15:33.293854-07:00
description: "\u65B9\u6CD5\uFF1A C\u8A00\u8A9E\u3067\u306F\u3001`stderr` \u30B9\u30C8\
  \u30EA\u30FC\u30E0\u304C\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\u30FC\u30B8\u3092\u66F8\
  \u304D\u8FBC\u3080\u305F\u3081\u306B\u4F7F\u7528\u3055\u308C\u307E\u3059\u3002`printf`\u306B\
  \u3088\u308B\u6A19\u6E96\u51FA\u529B\u3078\u306E\u66F8\u304D\u8FBC\u307F\u3068\u306F\
  \u7570\u306A\u308A\u3001`stderr`\u3078\u306E\u66F8\u304D\u8FBC\u307F\u306F `fprintf`\
  \ \u3084 `fputs` \u3092\u4F7F\u7528\u3059\u308B\u3053\u3068\u304C\u3067\u304D\u307E\
  \u3059\u3002\u4EE5\u4E0B\u304C\u305D\u306E\u65B9\u6CD5\u3067\u3059\uFF1A."
lastmod: '2024-04-05T22:38:42.307134-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A C\u8A00\u8A9E\u3067\u306F\u3001`stderr` \u30B9\u30C8\u30EA\
  \u30FC\u30E0\u304C\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\u30FC\u30B8\u3092\u66F8\u304D\
  \u8FBC\u3080\u305F\u3081\u306B\u4F7F\u7528\u3055\u308C\u307E\u3059\u3002`printf`\u306B\
  \u3088\u308B\u6A19\u6E96\u51FA\u529B\u3078\u306E\u66F8\u304D\u8FBC\u307F\u3068\u306F\
  \u7570\u306A\u308A\u3001`stderr`\u3078\u306E\u66F8\u304D\u8FBC\u307F\u306F `fprintf`\
  \ \u3084 `fputs` \u3092\u4F7F\u7528\u3059\u308B\u3053\u3068\u304C\u3067\u304D\u307E\
  \u3059\u3002\u4EE5\u4E0B\u304C\u305D\u306E\u65B9\u6CD5\u3067\u3059\uFF1A."
title: "\u6A19\u6E96\u30A8\u30E9\u30FC\u3078\u306E\u66F8\u304D\u8FBC\u307F"
weight: 25
---

## 方法：
C言語では、`stderr` ストリームがエラーメッセージを書き込むために使用されます。`printf`による標準出力への書き込みとは異なり、`stderr`への書き込みは `fprintf` や `fputs` を使用することができます。以下がその方法です：

```c
#include <stdio.h>

int main() {
    fprintf(stderr, "This is an error message.\n");

    fputs("This is another error message.\n", stderr);
    
    return 0;
}
```

サンプル出力 (stderrに対して)：
```
This is an error message.
This is another error message.
```

コンソールでの出力は `stdout` に似ているように見えるものの、ターミナルでリダイレクションを使用した場合、その違いははっきりします：

```sh
$ ./your_program > output.txt
```

このコマンドは標準出力を `output.txt` にのみリダイレクトし、エラーメッセージは引き続き画面に表示されます。

## 深掘り
Unix系システムにおける `stdout` と `stderr` の区分は、C言語とUnixの初期の日々にさかのぼります。この区分により、プログラマーは標準プログラム出力とは独立してエラーメッセージをリダイレクトできるため、より堅牢なエラーハンドリングとログ取りが可能になります。`stderr` はデフォルトでバッファリングされずに即座にエラーメッセージの出力が可能であり、これはクラッシュやその他の重大な問題のデバッグに役立ちますが、`stdout` は典型的にはバッファリングされ、出力が遅れる可能性があります（例えば、プログラムの完了時や手動でのフラッシング時など）。

現代のアプリケーションでは、特にコマンドラインツールやサーバーアプリケーションにおいて、通常のログメッセージとエラーを区別することが重要であるため、`stderr` への書き込みは依然として関連性があります。しかし、GUIアプリケーションやより洗練されたログ取りメカニズムが必要な場合には、プログラマーはメッセージのフォーマット、出力先（例：ファイル、ネットワーク）、および重大度レベル（情報、警告、エラーなど）をより制御できる専用のログライブラリを使用することがあります。

`stderr` はC言語におけるエラーレポートのための基本的なメカニズムを提供しますが、プログラミングの実践の進化と高度なログフレームワークの利用可能性により、それはしばしば現代のエラーハンドリング戦略の出発点に過ぎません。
