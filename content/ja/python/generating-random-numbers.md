---
title:                "ランダムな数字を生成する"
html_title:           "Python: ランダムな数字を生成する"
simple_title:         "ランダムな数字を生成する"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ？

ランダムな数字を生成することで、データをより多様化し、偶然性をシミュレートすることができます。これにより、プログラムのテストやゲーム開発など、さまざまな用途で活用することができます。

## 使い方

```Python
# ランダムモジュールをインポート
import random

# 0から10の範囲のランダムな整数を生成
print(random.randint(0,10))

# リストからランダムに要素を選択
list = ["りんご", "バナナ", "オレンジ", "イチゴ"]
print(random.choice(list))

# 0から1までの範囲のランダムな小数を生成
print(random.random())
```

プログラムを実行すると、毎回異なる結果が得られることがわかります。また、シード値を指定することで、同じ結果を再現することも可能です。

## ディープダイブ

ランダムな数字を生成する方法はさまざまありますが、大きく分けて疑似乱数と真の乱数の2種類があります。

疑似乱数はアルゴリズムに基づいて生成されるため、完全なランダム性を持ちません。一方、真の乱数は外部要因によって生成されるため、より偶然性を持ちます。Pythonのrandomモジュールは疑似乱数を生成するため、通常の用途では問題ありませんが、暗号技術などでの使用は避けるべきです。

## 参考リンク

- [Python公式ドキュメント - ランダムモジュール](https://docs.python.org/ja/3/library/random.html)
- [GeeksforGeeks - Random number generation in Python](https://www.geeksforgeeks.org/random-number-generation-in-python/)
- [Qiita - Pythonでランダムな数値を生成する方法](https://qiita.com/tomotaka_ito/items/ae7be28a4308c4766c9f)
- [Python Tips - Random Number Generators in Python](https://pythontips.com/2017/12/04/random-number-generators-in-python/)