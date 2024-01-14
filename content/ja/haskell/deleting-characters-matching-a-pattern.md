---
title:    "Haskell: パターンにマッチする文字を削除する"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## なぜ
あるパターンにマッチした文字を削除することに関心を持つ理由は、テキスト処理を行う上で必要不可欠な操作の1つであるからです。例えば、不要な空白や特定の文字列を除去したり、データの整形を行う際に役立ちます。

## 方法
``` Haskell
-- 文字列から特定の文字を削除する関数
deleteChar :: Char -> String -> String
deleteChar x [] = []
deleteChar x (y:ys)
  | x == y = deleteChar x ys  -- マッチする文字を見つけたら再帰的に呼び出し、削除する
  | otherwise = y : deleteChar x ys -- マッチしない場合はそのまま出力する
```

入力: `Delete Char
Pattern Matching Sample
Function`

出力: `Dlete ChPtern Mtching Smple Function`

## ディープダイブ
上記の例で使用したパターンマッチを用いて、文字の削除を行う関数を自作しました。パターンマッチとは、文字列を走査して特定のパターンがある場合に特定の処理を行うことを指します。例では、再帰的に呼び出すことでマッチする文字を全て削除しています。このような再帰的な処理が可能なのは、Haskellが純粋関数型言語であるためです。純粋関数型言語とは、値を変更することなく参照透過性がある関数を使うことで、副作用を最小限に抑えることができる言語のことを指します。

## 併せて読みたい
- [Haskellのパターンマッチングについて](https://qiita.com/7shi/items/f8d1bb1fc3128c30e040)
- [Haskell入門](https://qiita.com/matsuo159/items/9edf99d50b5af55d9208)
- [Haskellでの再帰関数の書き方](https://matsu7874.hatenablog.com/entry/2019/01/12/022354)