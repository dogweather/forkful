---
title:    "Gleam: 正規表現を使用する"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## なぜ正規表現を使うのか?

正規表現は、パターンをマッチングしてテキストを検索や置換をするために使用される便利なツールです。通常の文字列操作だけではできない処理を、簡潔に記述することができます。

## 使い方

まずはGleamの正規表現ライブラリであるRegexモジュールをインポートします。

```Gleam
import gleam/regex
```

ここでは、簡単な文字列のマッチングの例を紹介します。下のコードは、与えられた文字列が"hello"とマッチするかどうかをチェックし、結果をコンソールに表示します。

```Gleam
let string = "Hello, world!"
let pattern = regex.compile("\\/w+", Help.nil)

let result = regex.match(pattern, string)

case result {
  Ok(just(pattern)) -> io.println("マッチしました")
  _ -> io.println("マッチしませんでした")
}
```

上記の例では、"hello"がパターンにマッチしないため、結果として"マッチしませんでした"と出力されます。

さらに複雑なパターンを指定することもできます。例えば、メールアドレスの形式をチェックする場合は以下のようになります。

```Gleam
let string = "example@example.com"
let pattern = regex.compile("\\w+@\\w+\\.\\w+", Help.nil)

let result = regex.match(pattern, string)

case result {
  Ok(just(pattern)) -> io.println("有効なメールアドレスです")
  _ -> io.println("無効なメールアドレスです")
}
```

上記の例では、"example@example.com"が有効なメールアドレスの形式にマッチするため、結果として"有効なメールアドレスです"と出力されます。

## ディープダイブ

正規表現は非常に柔軟で強力なツールですが、複雑なパターンを作成する際には注意が必要です。特定の言語やシステムでは、正規表現のエンジンが異なるため、同じパターンでも処理が異なることがあります。また、正規表現は文字列だけでなく、数字や特殊文字なども扱えるため、パターンを作成する際にはどのような入力データを想定するかを明確にすることが重要です。

さらに詳しく学習したい場合は、オンラインドキュメントやチュートリアル、書籍なども参考にすることをおすすめします。

## 詳細情報

正規表現についてもっと詳しく学びたい方は、以下のリンクを参考にしてください。

- [GleamのRegexモジュールドキュメント](https://gleam.run/modules/regex)
- [正規表現チュートリアル(w3schools)](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)
- [正規表現リファレンス(MDN)](https://developer.mozilla.org/ja/docs/Web/JavaScript/Guide/Regular_Expressions)

## 関連リンク

- [Gleam公式サイト](https://gleam.run/)
- [Gleam公式ドキュメント](https://gleam.run/getting-started)
- [Gleam公式GitHubリポジトリ](https://github.com/gleam-lang/gleam)