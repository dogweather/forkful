---
title:                "複素数の扱い方"
date:                  2024-01-26T04:40:10.429631-07:00
model:                 gpt-4-0125-preview
simple_title:         "複素数の扱い方"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/working-with-complex-numbers.md"
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