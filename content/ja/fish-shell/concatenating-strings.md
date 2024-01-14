---
title:    "Fish Shell: 文字列の連結"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ

文字列を連結することに何か意味があるのでしょうか？そもそもなぜ私たちは文字列を連結するのでしょうか？

文字列を連結することは、プログラミングにおいて非常に重要です。例えば、ファイルパスやURLなどの文字列を連結することで、より複雑な処理を行うことができます。また、ある文字列を変数に代入する際にも、その値を文字列として連結することで、より柔軟なコードを記述することができます。

## 方法

では、実際にFish Shellで文字列を連結する方法を見ていきましょう。以下のコードブロックを参考にしてください。

```Fish Shell
# 文字列を連結する
set str1 "Hello"
set str2 "world"
echo $str1$str2

# 変数を文字列として連結する
set name "John"
set age 25
echo "My name is "$name" and I am "$age" years old."
```

以上のように、`$`を使用して変数を連結することで、より簡単に文字列を連結することができます。

実行結果は以下のようになります。

```Shell
Hello world
My name is John and I am 25 years old.
```

## 深堀り

文字列を連結する方法はさまざまありますが、Fish Shellでは特に便利なコマンドが用意されています。

まずは、`string join`コマンドです。これを使用することで、複数の文字列を指定した区切り文字で連結することができます。

例えば、`string join " " "hello" "world"`というコマンドを実行すると、`hello world`という文字列が作られます。

また、文字列を連結する際に、よく使用されるシングルクォーテーション(`'`)やダブルクォーテーション(`"`)も意識しておくことが重要です。シングルクォーテーションでは、変数を展開せずにそのまま文字列として扱われますが、ダブルクォーテーションでは変数を展開することができます。

詳しいコマンドの使い方や注意点は、公式ドキュメントを参考にしてください。

## 関連リンク

- [Fish Shell における変数の操作方法](https://fishshell.com/docs/current/tutorial.html#tut_variables)
- [Fish Shell 公式ドキュメント](https://fishshell.com/docs/current/index.html)
- [String concat | Commandlinefu](https://www.commandlinefu.com/commands/using/string/concat)