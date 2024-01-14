---
title:    "Haskell: 正規表現の使用"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜ

正規表現を使うことが有益なのでしょうか？正規表現の利点をご紹介します。

正規表現は非常に強力で、文字列を検索・置換・フィルタリングするために使われます。例えば、メールアドレスや電話番号、パスワードなどの書式をチェックするのに非常に便利です。

## 使い方

まずは、Haskellで正規表現を使う準備をしましょう。まずは、 `Text.Regex` モジュールをインポートします。

```Haskell
import Text.Regex
```

次に、文字列を検索するための正規表現パターンを定義します。以下は、"Haskell"という文字列を含むかどうかをチェックする簡単な例です。

```Haskell
let pattern = "Haskell"
```

そして、検索する文字列を選択し、 `matchRegex` 関数を使ってパターンとマッチさせます。

```Haskell
let text = "Haskell is a functional programming language."
matchRegex pattern text
```

ここで、出力は `Just["Haskell"]` という結果が得られるはずです。これは、 `Just` 値コンストラクタによって、 `Just` 型の値をラップしていることを意味します。

正規表現には、様々なパターンがありますので、詳しくはドキュメントをご覧ください。

## 深堀り

正規表現は表現力が高く、文字列の検索だけでなく、置換や分割などもサポートしています。

また、文字列の開始や終了をチェックするための特殊なパターンなども存在します。正規表現を使っていると、より複雑な文字列処理も容易になるでしょう。

しかしながら、正規表現はパフォーマンスの問題やバグの発生にも注意が必要です。正しく使わないと、文字列の処理が遅くなったり、意図しない結果を得る場合もあります。

## 参考

- [Haskellを使ってみよう（日本語訳）](http://learnyouahaskell.com/ja/chapters)
- [正規表現のチュートリアル（英語）](https://www.regular-expressions.info/tutorial.html)
- [正規表現の使い方（英語）](https://www.rexegg.com/)
- [正規表現ツールの比較（英語）](https://regex101.com/)