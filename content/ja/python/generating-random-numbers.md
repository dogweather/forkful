---
title:                "ランダム数の生成"
date:                  2024-01-20T17:50:08.631180-07:00
model:                 gpt-4-1106-preview
simple_title:         "ランダム数の生成"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
ランダムな数字を生成することは、予測不可能な数値を作り出すことです。プログラマーはテスト、データ解析、ゲームの開発などで、偶発性と多様性をもたらすためにこれを行います。

## How to:
Pythonでランダムな数字を生成する基本的な例を見てみましょう。

```python
import random

# 0から9までのランダムな整数を生成
random_integer = random.randint(0, 9)
print(random_integer)  # 例：4

# 0.0から1.0までのランダムな浮動小数点数を生成
random_float = random.random()
print(random_float)  # 例：0.437485123

# seedを設定して再現性を持たせる
random.seed(10)
print(random.random())  # 常に同じ値を出力、例：0.57140259469
```

## Deep Dive
ランダムな数値生成の歴史は古く、最初は物理的な方法（サイコロの振りなど）で行われてきました。コンピュータが出現してからはアルゴリズムを使って「擬似乱数（pseudo-random number）」を生成します。完全なランダム性は不可能ですが、我々の用途では十分です。他の方法としては、Pythonの`numpy`ライブラリの`numpy.random`モジュールを使うなどがあります。これは大量の数値を高速で生成する際に便利です。内部実装としては、`random`モジュールはメルセンヌ・ツイスターアルゴリズムを使用し、数値は擬似的にランダムですが、一般的な用途ではこれで十分です。

## See Also
- Pythonの公式ドキュメントで`random`モジュールについて詳しく知る: https://docs.python.org/3/library/random.html
- 科学技術計算でよく使われる`numpy.random`: https://numpy.org/doc/stable/reference/random/index.html
- よりセキュアな乱数生成には`secrets`モジュール: https://docs.python.org/3/library/secrets.html
