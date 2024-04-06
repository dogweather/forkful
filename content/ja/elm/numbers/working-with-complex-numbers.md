---
date: 2024-01-26 04:40:08.857339-07:00
description: "\u65B9\u6CD5\uFF1A Elm\u306F\u8907\u7D20\u6570\u3092\u30B5\u30DD\u30FC\
  \u30C8\u3057\u3066\u3044\u307E\u305B\u3093\u306E\u3067\u3001\u81EA\u5206\u306E\u578B\
  \u3068\u95A2\u6570\u3092\u4F5C\u6210\u3057\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u7C21\
  \u5358\u306A\u8A2D\u5B9A\u3067\u3059\uFF1A."
lastmod: '2024-04-05T21:53:42.886110-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u8907\u7D20\u6570\u306E\u6271\u3044\u65B9"
weight: 14
---

## 方法：
Elmは複素数をサポートしていませんので、自分の型と関数を作成します。以下は簡単な設定です：

```Elm
type alias Complex =
    { real : Float, imaginary : Float }

add : Complex -> Complex -> Complex
add a b =
    { real = a.real + b.real, imaginary = a.imaginary + b.imaginary }

-- 例：
a = { real = 3, imaginary = 2 }
b = { real = 1, imaginary = -4 }

sum = add a b
-- sum は { real = 4.0, imaginary = -2.0 }
```

## 掘り下げ
歴史的に、複素数は常に受け入れられていたわけではありません。16世紀になって、立方方程式を解くためのゲームチェンジャーとなりました。Pythonなど他の言語では、箱から出してすぐに使える複素数サポートと演算を提供しています。ご覧の通り、ElmではDIYのアプローチが必要です。しかし、掛け算、割り算、その他の演算を構築し、パフォーマンスの問題を調整することで、必要なほど洗練されたものにすることができます。

## 参照
- Elmの公式ドキュメント：https://package.elm-lang.org/ カスタムタイプの作成とElmの基本をマスターするために。
- 数学史に興味がある方は、Paul J. Nahin著の「An Imaginary Tale」で、複素数が時間を通じてどのように進化したかをチェックしてみてください。
- 複素数の魔術を活かす数学指向のプログラミングチャレンジには、プロジェクトオイラー（https://projecteuler.net）に挑戦してみてください。
