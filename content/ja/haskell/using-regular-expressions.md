---
title:                "Haskell: 正規表現を使用する"
simple_title:         "正規表現を使用する"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜ

正規表現は、文字列を検索、置換、パターンマッチングするための強力なツールです。Haskellでは、正規表現を使用することで、より複雑な処理を簡単に行うことができます。これは非常に便利なプログラミングツールであり、多くの場面で活用されています。

## 使い方

正規表現を使うには、まず`Text.Regex.Posix`モジュールをインポートする必要があります。次に、`=~`演算子を使用して文字列の中から特定のパターンを検索することができます。以下は、`Hello`という文字列から`Hell`というパターンを検索するコード例です。

```Haskell
import Text.Regex.Posix

main = do
    let helloString = "Hello"
    if helloString =~ "Hell"
        then putStrLn "パターンが見つかりました！"
        else putStrLn "パターンは見つかりませんでした。"
```

上記のコードの出力は、`パターンが見つかりました！`となります。

## 深堀り

正規表現は、検索パターンを表す文字列の特定の形式を使用します。これにより、より高度な検索が可能になります。例えば、`Hello`という文字列から`Hello`という単語を見つけるのではなく、`Hello`から始まる単語を見つけたい場合は、`Hello`の後に` .*`を追加することで、`Hello`という単語の後ろに何らかの文字が続く場合もすべてを検索することができます。

また、正規表現の中に`()`を使用することで、グループ化を行うことができます。これにより、特定の部分のみを抽出することができます。

正規表現の書き方は難しく感じるかもしれませんが、実際にコーディングをしていくうちに理解が深まっていきます。練習を重ねることで、より複雑な正規表現もスムーズに書くことができるようになります。

## See Also
- [Haskell正規表現チュートリアル](https://qiita.com/7shi/items/145f123a7faf7fbdd3aa)
- [正規表現の基礎知識 - Qiita](https://qiita.com/jnchito/items/893c887fbf19e17d3ff9)