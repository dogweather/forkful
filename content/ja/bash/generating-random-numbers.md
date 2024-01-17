---
title:                "乱数の生成"
html_title:           "Bash: 乱数の生成"
simple_title:         "乱数の生成"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 何 & なぜ？

ランダムな数字を生成することは、プログラマーがランダムな値を必要とする場合に使用されるテクニックです。例えば、ランダムなパスワードを作成したり、ゲームの乱数を生成したりするのに役立ちます。

# 方法：

ランダムな数字を生成するには、シェルスクリプトの中で```$RANDOM```変数を使用します。この変数は、-32768から32767の範囲内でランダムな値を返します。例えば、次のように使用します。

```Bash
echo $RANDOM
```
出力：

```Bash
7432
```
または、より具体的に数字の範囲を指定する場合は、次のようにします。

```Bash
echo $(($RANDOM % 100)) 
```
出力：

```Bash
59 
```

# 詳細を深く：

ランダムな数字を生成する方法には、他にも様々な方法があります。例えば、Bashの```/dev/random```デバイスを使用する方法や、Pythonなどのプログラミング言語で使用できる乱数生成関数を利用する方法があります。また、ランダムな数字を使用するアルゴリズムには、偽乱数生成アルゴリズムと真の乱数生成アルゴリズムの2種類があります。偽乱数生成アルゴリズムでは、ある固定された数列を元にランダムな数字を生成しますが、真の乱数生成アルゴリズムでは、外部の要因や物理現象を元にランダムな数を生成するため、より安全性の高い方法と言えます。

# 関連情報：

- Bashのランダムな数字の生成方法について詳しくは、こちらの記事を参考にしてください：https://tldp.org/LDP/abs/html/randomvar.html
- 真の乱数と偽乱数について詳しくは、こちらの記事を参考にしてください：https://en.wikipedia.org/wiki/Random_number_generation