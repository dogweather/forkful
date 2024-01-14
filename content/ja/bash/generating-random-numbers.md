---
title:    "Bash: 乱数の生成"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ

ランダムな数値を生成する理由はさまざまですが、最も一般的なのはランダムなデータを必要とするプログラムを作成するためです。例えば、ゲームやシミュレーション、セキュリティテストなどが挙げられます。さらに、ランダムな数値を扱うことでプログラムの予測可能性を減らすことができるため、コンピューターセキュリティにおいても重要な役割を果たしています。

## 方法

Bashスクリプトを使用して、簡単にランダムな数値を生成することができます。以下の例を参考にしてください。

```Bash 
#!/bin/bash
# 1から10までのランダムな数値を生成する方法
echo $(( $RANDOM % 10 + 1 ))

# 10から20までのランダムな数値を生成する方法
echo $(( $RANDOM % 11 + 10 ))
```

上記のコードでは、`echo`コマンドと`$RANDOM`変数を使用してランダムな数値を生成しています。`%`演算子は余りを求めるために使用し、最後にランダムな範囲の最小値を加算しています。この方法を使用すれば、任意の範囲のランダムな数値を生成することができます。

## ディープダイブ

ランダムな数値を生成する際に注意すべき点は、実際には完全にランダムではないことです。コンピューターは擬似乱数と呼ばれるアルゴリズムを使用して数値を生成するため、数値のパターンが発生する可能性があります。また、`$RANDOM`変数の最大値は32767であるため、それ以上の数値を生成することはできません。

このような問題を解決するためには、より高度な擬似乱数生成器を使用する必要があります。Bashでは、`/dev/random`や`/dev/urandom`といった特殊ファイルを使用することで、より高品質なランダムな数値を生成することができます。詳細については、[こちらの記事](https://wiki.bash-hackers.org/commands/builtin/rand)を参考にしてください。

## 関連リンク

- [Bashハッカーズウィキ - $RANDOM変数](https://wiki.bash-hackers.org/commands/builtin/rand)
- [Bashドキュメント - 乱数生成器の使い方](https://linuxhint.com/generate_random_number_bash/)
- [Guide to Bash Random Numbers](https://www.baeldung.com/linux/bash-random-numbers)