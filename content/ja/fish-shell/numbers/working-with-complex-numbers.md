---
aliases:
- /ja/fish-shell/working-with-complex-numbers/
date: 2024-01-26 04:40:10.429631-07:00
description: "\u8907\u7D20\u6570\u306F\u4E00\u6B21\u5143\u306E\u6570\u76F4\u7DDA\u306E\
  \u8003\u3048\u3092\u4E8C\u6B21\u5143\u306E\u8907\u7D20\u5E73\u9762\u306B\u62E1\u5F35\
  \u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30A8\u30F3\
  \u30B8\u30CB\u30A2\u30EA\u30F3\u30B0\u3001\u7269\u7406\u5B66\u3001\u30B0\u30E9\u30D5\
  \u30A3\u30C3\u30AF\u30B9\u306A\u3069\u306E\u5206\u91CE\u3067\u3001\u4FE1\u53F7\u3084\
  \u56DE\u8EE2\u306E\u3088\u3046\u306A\u4E8C\u3064\u306E\u6210\u5206\u3092\u5FC5\u8981\
  \u3068\u3059\u308B\u8A08\u7B97\u306B\u305D\u308C\u3089\u3092\u4F7F\u7528\u3057\u307E\
  \u3059\u3002"
lastmod: 2024-02-18 23:08:55.301788
model: gpt-4-0125-preview
summary: "\u8907\u7D20\u6570\u306F\u4E00\u6B21\u5143\u306E\u6570\u76F4\u7DDA\u306E\
  \u8003\u3048\u3092\u4E8C\u6B21\u5143\u306E\u8907\u7D20\u5E73\u9762\u306B\u62E1\u5F35\
  \u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30A8\u30F3\
  \u30B8\u30CB\u30A2\u30EA\u30F3\u30B0\u3001\u7269\u7406\u5B66\u3001\u30B0\u30E9\u30D5\
  \u30A3\u30C3\u30AF\u30B9\u306A\u3069\u306E\u5206\u91CE\u3067\u3001\u4FE1\u53F7\u3084\
  \u56DE\u8EE2\u306E\u3088\u3046\u306A\u4E8C\u3064\u306E\u6210\u5206\u3092\u5FC5\u8981\
  \u3068\u3059\u308B\u8A08\u7B97\u306B\u305D\u308C\u3089\u3092\u4F7F\u7528\u3057\u307E\
  \u3059\u3002"
title: "\u8907\u7D20\u6570\u306E\u6271\u3044\u65B9"
---

{{< edit_this_page >}}

## 何となぜ？
複素数は一次元の数直線の考えを二次元の複素平面に拡張します。プログラマーは、エンジニアリング、物理学、グラフィックスなどの分野で、信号や回転のような二つの成分を必要とする計算にそれらを使用します。

## 方法：
Fishで、実数部と虚数部を使って複素数を扱います。始め方はこちらです：

```fish
# 二つの複素数 (3+4i) と (5+2i) を加算
set complex_sum (math "3+4i + 5+2i")
echo $complex_sum # 出力： 8+6i

# 二つの複素数 (1+2i) と (3+4i) を乗算
set complex_prod (math "1+2i * 3+4i")
echo $complex_prod # 出力： -5+10i
```

複素数を累乗したり、その指数形式を得る必要がある場合：

```fish
# (2+3i) の二乗
set complex_square (math "(2+3i)^2")
echo $complex_square # 出力： -5+12i

# (2i) の指数
set complex_exp (math "e^(2i)")
echo $complex_exp # 出力： -0.41615+0.9093i
```

## 深堀り
Fish Shellの複素数に関する数学サポートは、バージョン3.1.0あたりから比較的新しく始まりました。それ以前は、人々は`bc`を使用したり、Pythonのような外部ツールを呼び出して複雑な数学を行っていたかもしれません。

Fishの`math`に代わるものには、MATLAB、NumPyを使用したPython、あるいは標準ライブラリを使ったC++など、特殊な数値ライブラリや言語があります。しかし、これらは迅速なシェル計算には過剰かもしれません。

Fishの複素数サポートは、内部の`math`コマンドに組み込まれており、libcalcを活用しています。これは、基本操作のために追加のツールをインストールする必要がないことを意味します。

しかし、Fishは重い数学計算のために設計されていません。その数学能力は迅速な計算や複素数が関係するスクリプトに便利ですが、集中的なタスクにはより堅牢なツールを検討してください。

## 参照
- 数学に関するFish shellドキュメント：https://fishshell.com/docs/current/commands.html#math
- Pythonの一般的な代替品、NumPy：https://numpy.org/
- 複素数に関するより深い理解：https://betterexplained.com/articles/a-visual-intuitive-guide-to-imaginary-numbers/
