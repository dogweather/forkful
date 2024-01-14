---
title:    "C++: テキストの検索と置換"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なぜ

プログラミングにおいて、テキストの検索と置換は非常に重要なタスクです。例えば、大きなテキストファイルや複数のファイルの中から特定のキーワードを探したり、間違っている単語やパスワードを一括で置き換えたりすることができます。これにより、パフォーマンスの向上や不要な手作業の省略が可能となります。

## 使い方

まず、C++のSTLライブラリーである`<algorithm>`をインクルードします。
```
#include <algorithm>
```

次に、検索する文字列と置換する文字列を変数に代入します。
```
string target = "こんにちは";
string replace = "Hello";
```

検索と置換の方法には、`replace()`や`search()`といった関数があります。
```
replace(target.begin(), target.end(), 'こんにちは', 'Hello'); // 全ての「こんにちは」を「Hello」に置換
search(target.begin(), target.end(), 'こんにちは'); // 最初に見つかった「こんにちは」の位置を返す
```

これらの関数は、単語や文字列の他にもパターンマッチングを行うこともできます。詳細な使い方は、ドキュメントを参照してください。

## ディープダイブ

検索と置換のアルゴリズムは、線形検索やボーイヤー・ムーア法といったアルゴリズムを使って実装されています。これらのアルゴリズムは、処理速度やメモリ使用量の観点から最適なものが選択されています。また、検索や置換のオプションとして、大文字・小文字の区別や部分一致の許容などを指定することもできます。

## 参考リンク

- [C++の検索と置換について](https://cpprefjp.github.io/reference/algorithm/)
- [アルゴリズムの説明と実行時間の比較](https://atcoder.jp/contests/dp/submissions/5499326)
- [C++での文字列処理の基本](https://qiita.com/gglnnski/items/4f9051849bb7e1894b7c)