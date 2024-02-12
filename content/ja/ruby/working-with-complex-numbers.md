---
title:                "複素数の扱い方"
aliases:
- ja/ruby/working-with-complex-numbers.md
date:                  2024-01-26T04:45:58.016741-07:00
model:                 gpt-4-0125-preview
simple_title:         "複素数の扱い方"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## 何となぜ？
複素数は、実部と虚部（3+4iのように）で構成され、工学や物理学で基本的な要素です。プログラマーは、単に実数だけでは上手くいかないシミュレーション、信号処理、方程式の解決に彼らを使用します。

## どのようにして：
Rubyは複素数の取り扱いを容易にします。Complexクラスを使用して作成および操作することができます：

```ruby
require 'complex'

# 複素数を作成
c1 = Complex(3, 4)
c2 = Complex('2+5i')

# 基本操作
sum = c1 + c2               # => (5.0+9.0i)
difference = c1 - c2        # => (1.0-1.0i)
product = c1 * c2           # => (-14.0+23.0i)
quotient = c1 / c2          # => (0.896551724137931+0.03448275862068961i)

# 共役、絶対値、および位相
conjugate = c1.conjugate    # => (3.0-4.0i)
magnitude = c1.abs          # => 5.0
phase = c1.phase            # Math.atan2(4, 3) => 0.9272952180016122 ラジアン

# 複素数特有のメソッド
polar = c1.polar            # => [5.0, 0.9272952180016122]
rectangular = c1.rect       # => [3.0, 4.0]
```

## 詳細について
複素数は新しいものではありません。16世紀から存在し、実解がない方程式の解決に使われてきました。数学を除けば、計算上、RubyのComplexクラスが重労働を引き受け、三角関数や超越関数のためのMathモジュールによって支えられます。

以前のプログラミング言語は、実部と虚部の手動扱いを必要としました。いくつかの言語、例えばFortranやC++は、複素数算術に特別なライブラリを当てています。

Rubyのアプローチは、構文に複素数サポートを組み込むことで、車輪の再発明からあなたを解放します。舞台裏では、Complexクラスが数学を処理し、Rubyがオブジェクト相互作用を世話します。

## 参照
- Ruby Docs on Complex: [https://ruby-doc.org/core/Complex.html](https://ruby-doc.org/core/Complex.html)
- MathWorld's take on Complex Numbers: [http://mathworld.wolfram.com/ComplexNumber.html](http://mathworld.wolfram.com/ComplexNumber.html)
- 複素数とその有用性についての視覚的紹介: [https://www.youtube.com/watch?v=5PcpBw5Hbwo](https://www.youtube.com/watch?v=5PcpBw5Hbwo)
