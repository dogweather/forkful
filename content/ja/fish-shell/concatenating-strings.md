---
title:    "Fish Shell: 文字列の連結"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

＃＃ なぜ
文字列の連結に取り組む理由を説明します。

文字列を連結することは、多くのプログラミング言語で非常に一般的な操作です。それは、複数の文字列を一緒に結合し、1つの文字列にすることを意味します。この操作を行う理由は、メッセージの構築やファイルパスの作成など、さまざまな場面で活用することができるからです。

＃＃ 方法
Fish Shellで文字列を連結する方法を紹介します。

```Fishシェル
set greeting "こんにちは"
set name "太郎"
set message $greeting$name
echo $message
```

出力：こんにちは太郎

上記の例では、まず2つの文字列をそれぞれ`greeting`と`name`という変数に代入しました。`greeting`には「こんにちは」という文字列が、`name`には「太郎」という文字列が格納されています。そして、`message`という変数に`$greeting`と`$name`を連結しています。最後に、`echo`コマンドを使用して変数`message`の値を出力しています。

＃＃ 深堀り
文字列を連結する際には、注意すべきポイントがいくつかあります。

- Fish Shellでは、変数を連結する際に`$`を付ける必要があります。付け忘れると、変数名自体が文字列として連結されてしまいます。
- 文字列の連結には`set`コマンドを使用することができますが、別の方法としては`sprintf`関数を使用する方法もあります。こちらはC言語でよく使われる書式指定子を使用することができます。

＃＃ 参考リンク
- [Fish Shell 公式ドキュメント](https://fishshell.com/docs/current/cmds/set.html)
- [Fish Shellで文字列の連結をする方法](https://qiita.com/sakaimo/items/2073a80418ea0701efe9)
- [Fish Shellの変数と文字列の操作](https://qiita.com/fisherman/items/f7a0d62a17cabf8f6b76)

＃＃ 参考になるリンク
- [データ型と操作 - 文字列の連結 (PHP)](https://www.php.net/manual/ja/language.operators.string.php)
- [文字列の連結 (Python)](https://docs.python.org/ja/3/tutorial/introduction.html#strings)