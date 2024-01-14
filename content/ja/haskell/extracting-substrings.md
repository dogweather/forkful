---
title:    "Haskell: 文字列の抽出"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## なぜ
文字列からサブストリングを抽出することの理由を1-2文で説明します。

## 方法
下記の「```Haskell ... ```」コードブロック内に、コーディング例と出力のサンプルを記載します。

```Haskell
-- 文字列を定義
let str = "こんにちは、世界！"

-- サブストリングを抽出
let substring = take 5 (drop 4 str)

-- 出力：世界
substring
```

## 深堀り
サブストリングを抽出するためのさまざまな関数があります。例えば、`take`関数は指定された数の要素をリストの前から取得し、`drop`関数は指定された数の要素をリストの前から削除します。これらの関数を組み合わせることで、特定の位置から指定した長さのサブストリングを抽出することができます。また、`splitAt`関数を使用すれば、指定された位置で文字列を分割し、それぞれの部分を取得できます。

## 関連リンク
サブストリングを抽出する際に役立つ関数や実践的な例を以下のリンクから参考にしてみてください。

- [Haskell言語の参考書「実践的Haskellプログラミング」](http://www.geocities.jp/m_hiroi/haskell/abc/haskell08.html)
- [Haskellで文字列を扱う方法](https://qiita.com/7shi/items/8813fcb1b45c43f89148)
- [Haskellの基礎知識](https://qiita.com/south37/items/6c6420f7a88cd32f891e)