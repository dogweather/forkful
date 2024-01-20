---
title:                "文字列の連結"
html_title:           "Bash: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列の連結は、二つ以上の文字列を一つに結合する真ん中の項目です。プログラマはこれを使って情報を組み合わせます。

## 手順

Fish Shellコード例と出力を見てみましょう:

```
>set str1 "フィッシュ"
>set str2 "シェル"
>set concatenated $str1$str2
>echo $concatenated  
```
出力:
```
フィッシュシェル
```
上記の例では、実行結果が"フィッシュシェル"という新たな文字列です。

## より深く 

Fish Shell は歴史的にも新しいコマンドラインシェルで、他のシェルと比較するといくつかの特性があります。文字列の連結の方法は例外ではありません。 連結に特定の演算子が必要なシェルもありますが、Fishでは変数名とその次の文字列や変数の間にスペースが無い限り自動で連結します。

もし他に対処方法が必要なら、`string join`関数を使うこともできます。とても有用な機能で、配列の各要素を一つの文字列に連結することができます。

## 参考資料

更なる情報は、以下のリンクをご覧ください：

- [公式Fishドキュメンテーション](https://fishshell.com/docs/current/index.html)
- [Fish Shellチュートリアル](https://fishshell.com/docs/current/tutorial.html)

このように、Fish Shellは簡単な操作で文字列の連結が可能であり、様々な仕事の流れを合理化することができます。