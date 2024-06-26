---
date: 2024-01-26 04:45:58.016741-07:00
description: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A Ruby\u306F\u8907\u7D20\
  \u6570\u306E\u53D6\u308A\u6271\u3044\u3092\u5BB9\u6613\u306B\u3057\u307E\u3059\u3002\
  Complex\u30AF\u30E9\u30B9\u3092\u4F7F\u7528\u3057\u3066\u4F5C\u6210\u304A\u3088\u3073\
  \u64CD\u4F5C\u3059\u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\uFF1A."
lastmod: '2024-04-05T22:38:42.335313-06:00'
model: gpt-4-0125-preview
summary: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A Ruby\u306F\u8907\u7D20\u6570\
  \u306E\u53D6\u308A\u6271\u3044\u3092\u5BB9\u6613\u306B\u3057\u307E\u3059\u3002Complex\u30AF\
  \u30E9\u30B9\u3092\u4F7F\u7528\u3057\u3066\u4F5C\u6210\u304A\u3088\u3073\u64CD\u4F5C\
  \u3059\u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\uFF1A."
title: "\u8907\u7D20\u6570\u306E\u6271\u3044\u65B9"
weight: 14
---

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
