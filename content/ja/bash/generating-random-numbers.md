---
title:                "ランダムな数字の生成"
html_title:           "C#: ランダムな数字の生成"
simple_title:         "ランダムな数字の生成"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

# ランダム数字の生成について

## 何となぜ?

ランダム数字の生成は、求められた範囲内の任意の数値を生成するプロセスです。この機能は、テストデータの生成や、ゲームの中での予測不能な結果を出すためなどによく利用されます。

## どうやって:

以下のコードを見てみましょう。

```Bash
# 1から100までのランダムな数を生成
echo $((RANDOM % 100 + 1))
```

実行結果は以下の通りです。

```Bash
72
```

この場合、'72' は生成されたランダムな数値の一例です。

## 詳細情報:

ランダム数字の生成は、コンピュータプログラムが生まれたときから存在しています。Bashは内蔵の変数 'RANDOM' を使用してランダム数を生成します。この方法以外にも、`/dev/random` や `/dev/urandom`デバイスを利用する方法、`openssl`コマンドを使用する方法など、さまざまな方法があります。

`RANDOM`の内部的な仕組みは、擬似乱数ジェネレータ (PRNG) に基づいています。各生成は次の数を予測するために前の数に依存します。

## 参照資料:

1. BashマニュアルのRANDOMセクション: https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Bash-Variables
2. `openssl` コマンドとランダム生成: https://www.openssl.org/docs/man1.1.1/man1/openssl-rand.html
3. `/dev/random` と `/dev/urandom` の違い: https://www.2daygeek.com/linux-dev-random-vs-dev-urandom-difference/