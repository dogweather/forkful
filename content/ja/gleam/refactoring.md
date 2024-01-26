---
title:                "リファクタリング"
date:                  2024-01-26T01:18:52.164607-07:00
model:                 gpt-4-0125-preview
simple_title:         "リファクタリング"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/refactoring.md"
---

{{< edit_this_page >}}

## 何となぜ？
リファクタリングとは、外部の振る舞いを変えることなく、コードをよりクリーンで、保守しやすくするために再作業するプロセスです。プログラマーは、可読性を高め、複雑性を減らし、将来の更新や機能追加にコードベースをより適応しやすくするためにリファクタリングを行います。

## 方法：
複数の関数で繰り返し計算や文字列操作を行っているコードの塊があるとしましょう。それはリファクタリングの主要な対象です。ここでは、型安全性と不変性に強い重点を置いているGleamを使用した前後の例を紹介します：

```gleam
// リファクタリング前
pub fn calculate_area(width: Int, height: Int) -> Int {
  width * height
}

pub fn print_area(width: Int, height: Int) {
  let area = calculate_area(width, height)
  io.println("The area is \(area)")
}

// リファクタリング後
pub fn calculate_area(width: Int, height: Int) -> Int {
  width * height
}

pub fn print_area(area: Int) {
  io.println("The area is \(area)")
}

// 別のコードの部分で、print_areaをこのように呼び出します：
print_area(calculate_area(10, 20))
```

サンプル出力：
```
The area is 200
```

リファクタリングによって、`print_area` を単に印刷することにより集中させ、計算は他の場所で処理されるようにし、コードをよりモジュラーにし、再利用またはテストしやすくしました。

## 深堀り
リファクタリングという概念は、プログラミング自体が存在している限り存在しており、コードを見直して清掃することは良い家事の一部です。リファクタリングの現代的な形式化、および今日使用されている多くのテクニックやパターンは、1999年に出版されたMartin Fowlerの画期的な本「Refactoring: Improving the Design of Existing Code」にまで遡ることができます。

Gleamエコシステム内でのリファクタリングには特有の考慮事項があります。最も重要なのは、コンパイル時に厳格な型チェックが行われることで、物事を動かしているときに早期に間違いを捕捉できることです。Gleamのパターンマッチングと不変性の機能は、リファクタリングの主要な目標の一つである、より明確で簡潔なコードを記述するための指針としても機能します。

リファクタリングの代替案には、ゼロからのコードの書き直しや、迅速な修正でコードをパッチすることが含まれるかもしれません。しかし、リファクタリングは、インクリメンタルで、よく下線が引かれ、振る舞いを保持する変形を伴うため、新しいバグを導入せずに既存のコードを改善するための最も安全で効率的なアプローチとされています。

## 関連資料
- Martin Fowlerの「Refactoring」の書籍: https://martinfowler.com/books/refactoring.html
- Gleam言語のウェブサイト、追加のドキュメントや例を含む: https://gleam.run/
- 言語全般に適用可能な基本原理についてのMartin Fowlerによる「Refactoring: Improving the Design of Existing Code」: https://martinfowler.com/books/refactoring.html