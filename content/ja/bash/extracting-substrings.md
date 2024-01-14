---
title:    "Bash: 文字列の抽出"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## なぜ
サブ文字列を抽出するのに何のためにエンゲージするのか、その理由を1-2文で説明します。

サブ文字列抽出は、特定の文字列から必要な情報を抽出するために使用されます。例えば、電話番号やメールアドレスなどの特定の形式のデータを抽出する際に便利です。

## 方法
ここでは、Bashスクリプトを使用してサブ文字列を抽出する方法を説明します。以下の例は、ドメイン名を抽出するためのコードです。

```Bash
str="www.example.com"
domain=${str:4:-4}
echo $domain
```

上記のコードの出力は、`example`となります。

## ディープダイブ
サブ文字列を抽出する方法について詳しく知りたい方のために、より深く掘り下げた情報を紹介します。Bashスクリプトでは、`${string:start:offset}`の形式を使用して、文字列の一部を抽出することができます。また、`-`を使用することで、文字列の末尾からのオフセットを指定することも可能です。

## See Also
もっとBashプログラミングの知識を深めたい方は、以下のリンクを参考にしてください。

- [Bash Guide for Beginners](https://tldp.org/LDP/Bash-Beginners-Guide/html/)
- [ Bash Reference Manual](https://www.gnu.org/software/bash/manual/bash.html)
- [Bash Substring Expansion](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html#Shell-Parameter-Expansion)