---
date: 2024-01-26 03:46:41.536044-07:00
description: "\u65B9\u6CD5\uFF1A Python\u3067\u306F\u3001`round()`\u306F\u5358\u306B\
  \u5C0F\u6570\u70B9\u3092\u5207\u308A\u6368\u3066\u308B\u308F\u3051\u3067\u306F\u3042\
  \u308A\u307E\u305B\u3093\u3002\u6B74\u53F2\u7684\u306B\u3001Python\u3092\u542B\u3080\
  \u591A\u304F\u306E\u4ED6\u306E\u8A00\u8A9E\u306F\u3001\u300C\u534A\u6570\u3092\u5076\
  \u6570\u306B\u4E38\u3081\u308B\u300D\u307E\u305F\u306F\u300C\u9280\u884C\u5BB6\u306E\
  \u4E38\u3081\u300D\u306B\u5F93\u3044\u307E\u3059\u3002\u3053\u308C\u306F\u3001\u91D1\
  \u878D\u8A08\u7B97\u3067\u91CD\u8981\u306B\u306A\u308B\u5408\u8A08\u3084\u5E73\u5747\
  \u306E\u7D2F\u7A4D\u8AA4\u5DEE\u3092\u6700\u5C0F\u9650\u306B\u6291\u3048\u307E\u3059\
  \u3002\u2026"
lastmod: '2024-04-05T22:50:55.496920-06:00'
model: gpt-4-0125-preview
summary: "\u4EE3\u66FF\u6848\u3068\u3057\u3066\u3001Python\u306E\u6570\u5B66\u30E2\
  \u30B8\u30E5\u30FC\u30EB\u304B\u3089`math.floor()`\u3068`math.ceil()`\u304C\u3042\
  \u308A\u3001\u6570\u5024\u3092\u6B21\u306E\u6574\u6570\u307E\u3067\u5F15\u304D\u4E0B\
  \u3052\u305F\u308A\u4E0A\u3052\u305F\u308A\u3057\u307E\u3059\u3002\u3057\u304B\u3057\
  \u3001\u7CBE\u5EA6\u304C\u6C42\u3081\u3089\u308C\u308B\u5834\u5408\u306F\u3001`decimal`\u30E2\
  \u30B8\u30E5\u30FC\u30EB\u306E`quantize()`\u304C\u4E38\u3081\u6319\u52D5\u3092\u6307\
  \u5B9A\u3055\u305B\u307E\u3059\u3002"
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
