---
title:                "Bash: 文字列の結合"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ

文字列を連結する理由は、より複雑なプログラムを作成し、必要な情報をより効率的に処理するためです。

## 作り方

最も簡単な方法は、コマンドプロンプトに `echo` コマンドを入力して、文字列を連結することです。例えば、 `echo "Hello" "World"` を入力すると、 `Hello World` という出力が得られます。

もっと複雑な例として、変数を使用して文字列を連結する方法もあります。以下のコードを入力してみましょう。

```Bash
name="太郎"
greeting="こんにちは、"
echo "${greeting}${name}さん"
```

このコードを実行すると、 `こんにちは、太郎さん` という出力が得られます。変数を使用することで、より柔軟に文字列を連結することができます。

## 深堀り

文字列を連結する際には、ダブルクォーテーションを使用することが重要です。ダブルクォーテーションは、文字列を一つの引数として認識させるために必要です。もし、ダブルクォーテーションを使用しない場合、複数の引数として認識されてしまい、意図した出力を得ることができません。

また、 `echo` コマンド以外にも文字列を連結する方法があります。例えば、 `printf` コマンドや `cat` コマンドを使用することもできます。それぞれのコマンドについても調べてみてください。

## 参考リンク
- [Bash リファレンスマニュアル](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameters.html#Shell-Parameters)
- [Linuxize: Bash Concatenate Strings](https://linuxize.com/post/bash-concatenate-strings/)
- [TecMint: The Power of Linux "echo" Command [with Practical Examples]](https://www.tecmint.com/echo-command-in-linux/)

## 参考になるリンク
- [Bash スクリプトの基礎: 制御構文](https://qiita.com/ko1nksm/items/dbf9fe0ac5979f7ad8a4)
- [シェルスクリプトを完全に理解するための入門集](https://qiita.com/terukizm/items/8cb4d816aea88ae3fd9b)