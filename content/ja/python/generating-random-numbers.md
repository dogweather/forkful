---
title:                "乱数の生成"
html_title:           "Python: 乱数の生成"
simple_title:         "乱数の生成"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 何？なぜ？
ランダムな数字を生成することは、プログラマーがプログラミング中に行う重要なタスクの一つです。ランダムな数字は、ゲームやシミュレーション、暗号化などのアプリケーションで使用されます。プログラマーは、ユーザーにとってランダムで不可予の値を生成する必要がある場合に、この技術を使用します。

## 方法：
Pythonでは、**random**モジュールを使用して、簡単にランダムな数字を生成することができます。下記のコードを使用して、10から20までのランダムな整数を生成する方法を見てみましょう。

```Python
import random
random_number = random.randint(10, 20)
print(random_number)
```

出力は以下のようになります。

```
16
```

また、小数点以下のランダムな数値を生成することもできます。下記のコードは、0から1までのランダムな小数点以下の数値を生成する例です。

```Python
import random
random_float = random.random()
print(random_float)
```

出力は以下のようになります。

```
0.782994658361
```

## 詳しく調べる：
ランダムな数字を生成するために、様々なアルゴリズムが使用されます。疑似乱数生成器と呼ばれるアルゴリズムは、事前に決められた種（seed）からランダムな数列を生成します。Pythonでは、初期化する種を指定することで、同じ数字の再現性を得ることができます。また、外部の物理現象を利用して本物のランダム数列を生成するハードウェアランダム数生成器もあります。

Pythonでランダムな数字を生成するための他の方法として、**random.uniform**関数や**random.choice**関数があります。詳しくは[公式ドキュメント](https://docs.python.org/ja/3/library/random.html)を参照してください。

## 関連リンク：
- [公式ドキュメント](https://docs.python.org/ja/3/library/random.html)
- [Pythonにおける疑似乱数生成器の仕組み](https://dozzie.jarowit.net/1998/12/python-random/)
- [ハードウェアランダム数生成器とは？](https://www.everythingrf.com/community/what-is-a-hardware-random-number-generator)