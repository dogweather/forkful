---
title:    "Go: 正規表現の使い方"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

### なぜ
正規表現を使用する理由を説明するために1-2文で説明します。

正規表現はテキストのパターンを検索し、一致する文字列を返すことができる非常に強力なツールです。これを使用することで、データのフィルタリングや検索をより効率的に行うことができます。

### 使い方
正規表現の使用方法を説明するために、いくつかのコーディング例とそれに対する出力を「```Go ... ```」のコードブロックで示します。

例1: メールアドレスの検証

```Go
pattern := `^[a-z0-9._%+\-]+@[a-z0-9.\-]+\.[a-z]{2,4}$`
valid := regexp.MustCompile(pattern).MatchString("example@email.com")
fmt.Println(valid) // true
```

例2: 電話番号の検索

```Go
pattern := `^(\(\d{3}\)\s?|\d{3}-)\d{3}-\d{4}$`
matches := regexp.MustCompile(pattern).FindStringSubmatch("My phone number is (123) 456-7890.")
fmt.Println(matches[1]) // (123)
fmt.Println(matches[2]) // 456-7890
```

正規表現を使用する際には、``Compile()``や``FindString()``, ``FindStringSubmatch()``といったメソッドを使用することで、パターンをコンパイルし、テキスト内での一致する部分を検索することができます。

### 詳細

更に詳細に正規表現を使用する際には、以下の点に注意してください。

- 一般的なユーザー入力ではなく、信頼できるデータにのみ正規表現を適用することを推奨します。
- 正規表現内の特殊文字やメタ文字について事前に学習することが重要です。

正規表現はパターンマッチングのための非常に強力なツールですが、学習コストが高いとも言われています。そのため、慣れるまでは他のメソッドを使用することも検討してみてください。

### 参考

正規表現の学習に役立つ以下のリンクを参考にしてみてください。

- [正規表現入門](https://qiita.com/jnchito/items/893c887fbf19e17d3ff9)
- [Goでの正規表現の使用方法](https://programmingistart.com/go/regular-expression-in-go/)
- [Goのレギュラーエクスプレッションパッケージドキュメント](https://golang.org/pkg/regexp/)

### 他の記事を参照

- [正規表現を使用したデータ検索のパフォーマンス改善策](https://blog.example.com/regex-performance-improvement-ja)
- [Google RE2とGoの正規表現エンジンの比較](https://engineer.example.com/re2-vs-go-regex-ja)