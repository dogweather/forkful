---
title:                "インタラクティブシェル（REPL）の使用"
date:                  2024-01-26T04:11:53.036761-07:00
model:                 gpt-4-0125-preview
simple_title:         "インタラクティブシェル（REPL）の使用"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## 何となぜ？
インタラクティブシェル、またはRead-Eval-Print Loop（REPL）は、即座にコードスニペットをテストできるリアルタイムコーディング環境を提供するツールです。プログラマーは開発、学習、デバッグ中に迅速なフィードバックのために使用します。

## どのようにして：
Cには組み込みのREPLがありませんが、サードパーティのツールを使用することができます。C言語のコードも扱えるC++インタプリタであるClingを使った一例をここに紹介します：

```C
#include <stdio.h>

int main() {
    printf("Hello, REPL world!\n");
    return 0;
}
```

Cling REPLでの出力：
```
[cling]$ .x yourscript.c
Hello, REPL world!
```

Clingはスクリプトを実行し、即座に出力を表示します。

## 深く掘り下げて
REPLはPythonやRubyのような動的言語では標準ですが、Cのようなコンパイル言語では一般的ではありません。歴史的に、コンパイル-実行-デバッグのサイクルはインタラクティブな探求に適していないとされてきました。ClingやオンラインCコンパイラーなどのツールは、CコードをC++環境でラップすることにより、REPLのような体験を提供します。

Clingの代替としては、CINTやChなどのCインタプリターがあります。これらのツールは迅速な繰り返しを可能にしますが、パフォーマンス制約や複雑な機能のサポートのために、すべての開発シナリオに適しているわけではありません。

コンパイル言語でのREPLの実装は、即座にコードスニペットをコンパイルして実行することを含み、これは単純ではなく、完全な言語機能に比べて制限がある可能性があります。

## 参照
- Cling: https://github.com/root-project/cling
- オンラインCコンパイラーとREPL: https://repl.it/languages/c
- CINT: http://root.cern.ch/drupal/content/cint
- Chインタプリター: http://www.softintegration.com/products/chstandard/