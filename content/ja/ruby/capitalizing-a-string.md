---
title:                "Ruby: 文字列の大文字変換"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列を大文字にすることにも意味がありますか？その理由を説明します！

文字列を大文字にすることは、テキストを見やすくするためや、検索エンジンでの検索を容易にするために行われます。例えば、あなたの名前を大文字で書くことで、あなたの名前を探すのが簡単になります。また、日本語の場合、英語と比べて大文字と小文字の区別がないため、大文字にすることでテキストを強調することができます。

## やり方

文字列を大文字にするには、Rubyのcapitalizeメソッドを使用します。下の例を参考にしてください。

```Ruby
name = "yamada taro"
puts name.capitalize
```

出力結果は "Yamada taro" になります。

文字列だけでなく、変数やメソッド名にもcapitalizeメソッドを使用することができます。しかし、大文字にするには文字列が英語である必要があります。日本語の場合、発音が変わるため、使うことができません。

## 深堀り

実は、Rubyのcapitalizeメソッドは文字列の先頭文字を大文字にするだけでなく、先頭以外の文字を全て小文字にします。例えば、"YAMADA Taro"という文字列をcapitalizeすると、"Yamada taro" になります。

また、文字列全体を大文字にするには、upcaseメソッドを使用します。下の例を参考にしてください。

```Ruby
name = "yamada taro"
puts name.upcase
```

出力結果は "YAMADA TARO" になります。

## 今後も参考にしてね

この記事を読んで、文字列を大文字にする方法を学びました。UNIXコマンドやJavaScriptでも同じことができるので、ぜひ練習してみてください。

<!-- Japanese See Also -->
## 関連記事

- [Rubyの公式ドキュメント: String#capitalize](https://docs.ruby-lang.org/ja/latest/method/String/i/capitalize.html)
- [文字列を大文字に変換する方法 - TECHacademyマガジン](https://techacademy.jp/magazine/10150)
- [Rubyで文字列を大文字に変換する方法 - Qiita](https://qiita.com/hypermkt/items/df4c95850763e39e88be)