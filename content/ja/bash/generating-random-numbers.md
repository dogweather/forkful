---
title:                "Bash: 乱数の生成"
simple_title:         "乱数の生成"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ

ランダムな数字を生成することのメリットは何でしょうか？プログラミングにおいて、ランダムな数字は非常に重要です。たとえば、ゲームや乱数を必要とするアプリケーションを開発する際に、ランダムな数字を生成することで、よりリアルな体験を提供することができます。また、ランダムな数字を使うことで、予測不可能な動作を作り出すこともできます。さらに、ランダムな数字を生成することで、セキュリティやプライバシーの向上にもつながります。

## 方法

Bashでランダムな数字を生成する方法を紹介します。まず、次のコードを`script.sh`という名前で保存します。

```Bash
#!/bin/bash

# `$RANDOM`変数を使ってランダムな数字を生成する
echo "ランダムな数字: $RANDOM"
```

次に、ターミナルで`script.sh`を実行します。

```Bash
$ bash script.sh
```

すると、毎回異なるランダムな数字が表示されることを確認できるでしょう。

もし、特定の範囲内の数字をランダムに生成したい場合は、`$RANDOM`変数に`%`（剰余）演算子を使うことで可能です。例えば、1から10までのランダムな数字を生成するには、次のようにします。

```Bash
#!/bin/bash

# 1から10までのランダムな数字を生成する
echo "ランダムな数字: $((RANDOM % 10 + 1))"
```

## ディープダイブ

`$RANDOM`変数は、シェルスクリプトの実行時にシステムから取得された乱数を返します。ただし、この方法では、予測可能な数字のパターンが存在し、セキュリティ的に弱いという欠点があります。

より安全なランダムな数字を生成するには、`/dev/random`または`/dev/urandom`を使用します。これらのデバイスは、カーネルが管理するエントロピープールから乱数を生成するためのインターフェースです。具体的には、`/dev/random`は十分なエントロピーがある場合にしかデータを返さず、`/dev/urandom`は常にデータを返します。

例えば、次のコードでは、`/dev/random`からランダムなバイト列を読み取り、それを16進数に変換して表示しています。

```Bash
#!/bin/bash

# /dev/randomからランダムなバイト列を読み取り、16進数に変換して表示する
echo "ランダムな数字: $(od -An -N2 -tx1 /dev/random | tr -d " " | tr "\n" " ")"
```

## 関連情報

- [Bashのドキュメント](https://www.gnu.org/software/bash/)
- [Bashでの数値操作](http://www.linux-mag.com/id/357/)
- [ランダムな数字の生成とセキュリティ](https://www.eetimes.com/how-to-generate-random-numbers-in-software-part-1/)
- [dev/random vs dev/urandom](https://blog.ontoillogical.com/blog/2015/01/24/dev-random-dev-urandom/)

## 参考リンク

https://www.gnu.org/software/bash/
http://www.linux-mag.com/id/357/
https://www.eetimes.com/how-to-generate-random-numbers-in-software-part-1/
https://blog.ontoillogical.com/blog/2015/01/24/dev-random