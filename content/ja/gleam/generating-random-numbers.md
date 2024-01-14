---
title:                "Gleam: ランダム数の生成"
simple_title:         "ランダム数の生成"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ
乱数を生成するのはなぜ重要なのでしょうか？乱数は、計算機科学やデータ分析で広く使用され、さまざまな問題やアルゴリズムを解決するために必要です。それでは、Gleamで乱数をどのように生成するか見ていきましょう。

## 生成方法
乱数を生成するには、 `rand` モジュールを使用します。これには、 `random` 関数があります。この関数を使用すると、範囲内のランダムな整数を生成することができます。

```Gleam
import rand

// 0から10までのランダムな整数を生成
let num = rand.random(0, 10)

// 結果の出力
io.format("生成された乱数は {}", [num])
```

結果は以下のようになります。

```
生成された乱数は 6
```

もし、浮動小数点数を生成したい場合は、 `random_float` 関数を使用します。

```Gleam
import rand

// 0から1の間の浮動小数点数を生成
let num = rand.random_float()

// 結果の出力
io.format("生成された浮動小数点数は {}", [num])
```

結果は以下のようになります。

```
生成された浮動小数点数は 0.283
```

## 深い掘り下げ
Gleamでは、さまざまな種類の乱数を生成することができます。これらには、 `uuid` や `bytes` などがあります。また、 `random_seed` のような関数を使用することで、独自の乱数生成シードを指定することもできます。

Gleam公式ドキュメントには、これらの関数や生成方法の詳細な情報が記載されています。詳しくは、公式ドキュメントを参照してください。

## 参考リンク
- [Gleam公式ドキュメント](https://gleam.run/documentation/)
- [Gleam `rand`モジュールのGitHubリポジトリ](https://github.com/gleam-lang/rand)
- [Gleamでの乱数の生成方法について](https://medium.com/@saradege/gleam-%E3%81%A7%E3%81%AE%E4%B9%B1%E6%95%B0%E3%81%AE%E7%94%9F%E6%88%90%E6%96%B9%E6%B3%95%E3%81%AB%E3%81%A4%E3%81%84%E3%81%A6-94a9a5ee306c)