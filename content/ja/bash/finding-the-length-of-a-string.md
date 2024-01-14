---
title:                "Bash: 文字列の長さを見つける"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列の長さを求めることに興味を持つ理由は、簡単なタスクから複雑なプログラムまで、コーディングのあらゆる側面で利用できるためです。また、文字列の長さを知ることで、文字列を処理する上でのミスを防ぐことができます。

## 方法

文字列の長さを求めるには、いくつかの方法があります。例えば、`echo`コマンドに`$`をつけて文字列を囲めば、文字列の長さを表示することができます。`echo ${#string}`というコマンドで、変数`string`の文字列の長さを表示することができます。

```Bash
string="こんにちは"
echo ${#string}
```

出力結果は`5`となります。

また、`len`関数を使うこともできます。例えば、下記のようにして、`len`関数を定義し、引数として与えた文字列の長さを求めることができます。

```Bash
len () {
        echo ${#1}
}

string="こんにちは"
len $string
```

出力結果は`5`となります。

## ディープダイブ

さらに詳しく調べると、文字列の長さを求める方法として、`tr`コマンドによる処理があります。`tr`コマンドを使用することで、文字列から特定の文字を除去することができ、その後、`wc -c`コマンドで残った文字数を数えることができます。例えば、下記のようにします。

```Bash
string="Hello, World"
len=$(echo $string | tr -d "," | wc -c)
echo $len
```

出力結果は`11`となり、`Hello, World`の文字数を正しく求めることができます。

## 参考文献

- [Find length of a string in Bash](https://www.hacksparrow.com/string-length-in-bash.html)
- [How do you tell how long a string is in Bash?](https://stackoverflow.com/questions/17368067/how-do-you-tell-how-long-a-string-is-in-bash)