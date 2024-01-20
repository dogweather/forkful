---
title:                "ランダムな数字の生成"
html_title:           "C#: ランダムな数字の生成"
simple_title:         "ランダムな数字の生成"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Pythonでのランダム数字生成
こんにちは、皆さん。今回はPythonでランダムな数を生成する方法について学んでいきましょう。

## ランダムな数字って何？そしてその理由は？
ランダムな数とは、予測不可能な数のことを示します。プログラムの結果を予測不可能にする、ゲームのコンテキストで利用する、または情報のシャッフル・選択などのため、プログラマーはランダムな数を生成します。

## 実装方法
Pythonでランダムな数を生成するには、`random`という標準ライブラリを使用します。以下にその一部を紹介します。

```Python
import random

# 0.0から1.0までのランダムな浮動小数点数を生成します。
random_float = random.random()
print(random_float)

# 1から10までのランダムな整数を生成します。
random_integer = random.randint(1, 10)
print(random_integer)
```
上記のコードを実行すると以下のような出力が得られます。 

```
0.47411011327940105
3
```

出力は毎回異なることにご注意ください。

## ディープダイブ
- **歴史的背景**: ランダム数の生成は、統計学、暗号学、ゲーム理論など、様々な分野で用いられています。
- **代替手段**: `numpy`ライブラリの`numpy.random`モジュールを使うことで、より複雑なランダム数の生成が可能です。例えば、正規分布や二項分布などのランダムな数を生成します。
- **詳細**: Pythonの`random`モジュールでは、メルセンヌツイスターという高品質の疑似乱数生成アルゴリズムを用いています。

## 詳細情報
以下に、関連する情報源へのリンクを掲蓉します。これらのリンクから更なる詳細を学ぶことができます。

- [Python公式ドキュメンテーション: random --- 擬似乱数を生成する](https://docs.python.org/ja/3/library/random.html)
- [Wikipedia: メルセンヌツイスター](https://ja.wikipedia.org/wiki/%E3%83%A1%E3%83%AB%E3%82%BB%E3%83%B3%E3%83%8C%E3%83%84%E3%82%A4%E3%82%B9%E3%82%BF)
- [NumPy公式ドキュメンテーション: Random sampling (numpy.random)](https://numpy.org/doc/stable/reference/random/index.html)