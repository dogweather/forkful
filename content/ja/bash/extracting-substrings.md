---
title:    "Bash: 部分文字列の抽出"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ

文字列を抽出することをどうしてあなたはやるか、それがどのようにあなたに役立うかについて書かれたBashプログラミングのブログ投稿です。この記事では、文字列を抽出する方法、そしてそれについてさらに深く掘り下げる方法を紹介します。

## 抽出する方法

文字列を抽出するには、Bashの組み込みコマンドである`substr`を使用します。使用例を見てみましょう。

```Bash
# 文字列の最初から3文字を抽出する
string="こんにちは"
echo ${string:0:3}

# 出力: こん

# 文字列の3文字目から最後までを抽出する
string="こんにちは"
echo ${string:2}

# 出力: にちは
```

`substr`コマンドを使用することで、変数の値から一部の文字を抽出することができます。

## 深く掘り下げる

文字列を抽出する方法は簡単ですが、いくつかのトリックを覚えることでより柔軟に操作することができます。

- `:`の後の数字を省略すると、指定した位置から最後までの文字を抽出することができます。
- `:`の前の数字を負の値にすると、文字を後ろから数えることができます。
- `:`の後の数字を`-`にすると、指定した位置から最後までの文字を逆順に抽出することができます。

詳しい使い方は、Bashの公式ドキュメントを参照してください。

## さらに参考に

他にも有用なBashの文字列操作については、こちらの記事をチェックしてください。

- [Bash Hackers Wiki - 文字列操作](https://wiki.bash-hackers.org/syntax/pe)
- [Bashドリル - 文字列操作のテクニック](http://bash.drill.so/?chapter=string_manipulation)

---
参考リンク:

## 関連記事

- [文字列操作の基本 - substrコマンドの使用方法](https://www.example.com/substr-basics)
- [Bashプログラミングの基礎学習ガイド](https://www.example.com/bash-tutorial)