---
date: 2024-01-26 04:45:58.016741-07:00
description: "\u8907\u7D20\u6570\u306F\u3001\u5B9F\u90E8\u3068\u865A\u90E8\uFF083+4i\u306E\
  \u3088\u3046\u306B\uFF09\u3067\u69CB\u6210\u3055\u308C\u3001\u5DE5\u5B66\u3084\u7269\
  \u7406\u5B66\u3067\u57FA\u672C\u7684\u306A\u8981\u7D20\u3067\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3001\u5358\u306B\u5B9F\u6570\u3060\u3051\u3067\u306F\
  \u4E0A\u624B\u304F\u3044\u304B\u306A\u3044\u30B7\u30DF\u30E5\u30EC\u30FC\u30B7\u30E7\
  \u30F3\u3001\u4FE1\u53F7\u51E6\u7406\u3001\u65B9\u7A0B\u5F0F\u306E\u89E3\u6C7A\u306B\
  \u5F7C\u3089\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.843424-06:00'
model: gpt-4-0125-preview
summary: "\u8907\u7D20\u6570\u306F\u3001\u5B9F\u90E8\u3068\u865A\u90E8\uFF083+4i\u306E\
  \u3088\u3046\u306B\uFF09\u3067\u69CB\u6210\u3055\u308C\u3001\u5DE5\u5B66\u3084\u7269\
  \u7406\u5B66\u3067\u57FA\u672C\u7684\u306A\u8981\u7D20\u3067\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3001\u5358\u306B\u5B9F\u6570\u3060\u3051\u3067\u306F\
  \u4E0A\u624B\u304F\u3044\u304B\u306A\u3044\u30B7\u30DF\u30E5\u30EC\u30FC\u30B7\u30E7\
  \u30F3\u3001\u4FE1\u53F7\u51E6\u7406\u3001\u65B9\u7A0B\u5F0F\u306E\u89E3\u6C7A\u306B\
  \u5F7C\u3089\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002."
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
