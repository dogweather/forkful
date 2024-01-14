---
title:                "Python: ランダムな数字の生成"
programming_language: "Python"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ

乱数を生成する活用が必要な理由は、コンピュータープログラミングにおいて偶発的な出力をシミュレートする必要があるからです。このシミュレーションは、ゲームや統計的なシミュレーションなど多岐に渡ります。

## 方法

```Python
# ランダムモジュールをインポートする
import random

# 0〜9の範囲でランダムな整数を生成
random_num = random.randint(0, 9)
print(random_num)
# 結果例: 5

# 0〜1の範囲でランダムな小数を生成
random_float = random.random()
print(random_float)
# 結果例: 0.3456891829
```

## 注意深い

乱数は完全にランダムではありません。コンピューターのアルゴリズムによって生成されたものであり、特定のパターンが現れる可能性があります。そのため、本当にランダムなデータが必要な場合は、外部のハードウェアデバイスを使用する必要があります。

## See Also

- [Python公式ドキュメント](https://docs.python.org/ja/3/library/random.html)
- [ランダム数生成のアルゴリズムの種類](https://en.wikipedia.org/wiki/Random_number_generation)