---
date: 2024-01-26 03:45:11.696714-07:00
description: "\u65B9\u6CD5\uFF1A Elm\u306E`Basics`\u30E2\u30B8\u30E5\u30FC\u30EB\u306F\
  \u3001\u4E38\u3081\u306B\u4FBF\u5229\u306A\u95A2\u6570\u3092\u63D0\u4F9B\u3057\u3066\
  \u3044\u307E\u3059\uFF1A`round`\u3001`floor`\u3001\u305D\u3057\u3066`ceiling`\u3002\
  \u3053\u308C\u3089\u306E\u4F7F\u3044\u65B9\u306F\u4EE5\u4E0B\u306E\u901A\u308A\u3067\
  \u3059\u3002"
lastmod: '2024-03-13T22:44:42.001057-06:00'
model: gpt-4-0125-preview
summary: "Elm\u306E`Basics`\u30E2\u30B8\u30E5\u30FC\u30EB\u306F\u3001\u4E38\u3081\u306B\
  \u4FBF\u5229\u306A\u95A2\u6570\u3092\u63D0\u4F9B\u3057\u3066\u3044\u307E\u3059\uFF1A\
  `round`\u3001`floor`\u3001\u305D\u3057\u3066`ceiling`\u3002\u3053\u308C\u3089\u306E\
  \u4F7F\u3044\u65B9\u306F\u4EE5\u4E0B\u306E\u901A\u308A\u3067\u3059."
title: "\u6570\u5024\u306E\u4E38\u3081\u51E6\u7406"
weight: 13
---

## 方法：
Elmの`Basics`モジュールは、丸めに便利な関数を提供しています：`round`、`floor`、そして`ceiling`。これらの使い方は以下の通りです。

```elm
import Basics exposing (round, floor, ceiling)

-- 最も近い整数に丸める
round 3.14    --> 3
round 3.5     --> 4

-- 切り捨て
floor 3.999   --> 3

-- 切り上げ
ceiling 3.001 --> 4

-- 丸めずに小数点以下を切り捨てる
truncate 3.76 --> 3
```

Elmはまた、小数点以下の固定数に丸めるための`toLocaleString`も提供しています：

```elm
import Float exposing (toLocaleString)

-- 2小数点以下に丸める
toLocaleString 2 3.14159 --> "3.14"
```

## 深掘り
Elmは強く型付けされた関数型言語であり、副作用をアーキテクチャの「エッジ」に委ねます。これは、丸めのような関数は純粋で予測可能でなければならないということを意味します。歴史的に見て、丸めは浮動小数点の演算の不正確さを扱う多くのプログラミング言語で一般的な操作です。

Elmの丸めへのアプローチはストレートフォワードです - 関数は純粋であり、round、floor、そしてceilingの数学的定義に準拠しています。Elmは、特にファイナンスやグラフィックスの分野で、精度管理が頻繁に必要とされる共通のニーズに応えるために、組み込み関数を提供します。

Elmの組み込み関数に代わる方法としては、算術演算を使用したカスタム実装が考えられますが、標準ライブラリが既に効率的に仕事をしているため、不必要な複雑さが増します。

現在のバージョンでは、Elmはこれらの操作にJavaScriptの下位の浮動小数点数学を使用しており、IEEE 754標準と一致しているので、精度と潜在的な浮動小数点エラーを考慮する際にはこれを覚えておく必要があります。

## 参照
- Elm公式の`Basics`モジュールドキュメント：https://package.elm-lang.org/packages/elm/core/latest/Basics
- コンピューティングでの浮動小数点数の動作についての詳細な解説：https://floating-point-gui.de/
- Elm `Float`モジュールでのさらなる浮動小数点操作：https://package.elm-lang.org/packages/elm/core/latest/Float
