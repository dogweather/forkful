---
date: 2024-01-26 03:46:41.536044-07:00
description: "\u65B9\u6CD5\uFF1A \u4EE5\u4E0B\u306FPython\u3067\u6570\u5024\u3092\u4E38\
  \u3081\u308B\u65B9\u6CD5\u306B\u3064\u3044\u3066\u306E\u8A73\u7D30\u3067\u3059\uFF1A\
  ."
lastmod: '2024-04-05T21:53:42.446739-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u6570\u5024\u306E\u4E38\u3081\u51E6\u7406"
weight: 13
---

## 方法：
以下はPythonで数値を丸める方法についての詳細です：

```python
# 数値を最も近い整数に丸める
print(round(8.67))  # 出力: 9

# 指定された小数点以下の桁数に数値を丸める
print(round(8.67, 1))  # 出力: 8.7

# 偶数は下に丸められ、奇数は上に丸められる
print(round(2.5))  # 出力: 2
print(round(3.5))  # 出力: 4
```

## ディープダイブ
Pythonでは、`round()`は単に小数点を切り捨てるわけではありません。歴史的に、Pythonを含む多くの他の言語は、「半数を偶数に丸める」または「銀行家の丸め」に従います。これは、金融計算で重要になる合計や平均の累積誤差を最小限に抑えます。

代替案として、Pythonの数学モジュールから`math.floor()`と`math.ceil()`があり、数値を次の整数まで引き下げたり上げたりします。しかし、精度が求められる場合は、`decimal`モジュールの`quantize()`が丸め挙動を指定させます。

内部的に、`round()`は二進浮動小数点数を扱います。二進法で正確に表現できない小数があるので、`round(2.675, 2)`が期待通りに`2.68`にならないような驚きがあるかもしれません。高精度が必要な場合は`decimal`や`fractions`を使います。

## 参照
- Pythonの組み込み関数に関するドキュメント: https://docs.python.org/3/library/functions.html#round
- Decimal固定小数点と浮動小数点演算: https://docs.python.org/3/library/decimal.html
- Pythonの数学モジュール: https://docs.python.org/3/library/math.html
