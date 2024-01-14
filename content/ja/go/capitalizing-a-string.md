---
title:    "Go: 文字列を大文字にする"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列を大文字にすることに関わる理由は何でしょうか？Go言語において、文字列を大文字に変換することはテキスト処理において非常に役立ちます。例えば、ユーザーの入力を一貫性のある形式に揃えるためや、検索機能の強化を目的として、文字列を大文字に変換する必要が生じることがあります。

## 方法

文字列を大文字に変換するには、stringsパッケージ内のToUpper関数を使用します。具体的なコードは以下のようになります。

```Go
import "strings"

func main() {
    var input string = "hello world"
    var capitalized string = strings.ToUpper(input)
    fmt.Println(capitalized)
}
```

上記のコードでは、stringsパッケージをインポートし、"hello world"という文字列を大文字に変換して、最終的には "HELLO WORLD"という出力を得ます。このように、stringsパッケージには多くの文字列処理に関する関数が用意されており、ToUpper関数もその中の一つです。

## ディープダイブ

stringsパッケージのToUpper関数は、実際にはUnicodeコードポイントで動作します。つまり、アルファベットだけでなく、日本語やその他の言語の文字も大文字に変換することができます。しかし、注意点としては、Unicodeの特殊文字や結合文字の扱いに注意が必要です。また、ToUpper関数は元の文字列を変えずに新しい大文字の文字列を返すため、元の文字列を変更したい場合には注意が必要です。詳細な仕様は公式ドキュメントを参照してください。

## 主なリンク

- [Go言語公式ドキュメント: stringsパッケージ](https://golang.org/pkg/strings/)
- [Effective Go: stringsパッケージ](https://golang.org/doc/effective_go#strings)
- [無料プログラミング学習サイト: A Tour of Go - stringsパッケージ](https://go-tour-jp.appspot.com/methods/15)