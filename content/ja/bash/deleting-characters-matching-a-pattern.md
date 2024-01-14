---
title:                "Bash: パターンにマッチする文字を削除する"
simple_title:         "パターンにマッチする文字を削除する"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ？

Bashプログラミングを学ぶ上で、特定のパターンにマッチする文字を削除する方法を習得することは重要です。それにより、より効率的なスクリプトの作成や、データの処理が容易になります。

## 方法

特定のパターンにマッチする文字を削除するには、以下のようなコードを使用することができます。

```Bash
# 文字列を変数に代入
str="Hello World!"

# パターンにマッチする文字を削除
new_str=${str//l/}

# 出力
echo $new_str

# Output: Heo Word!
```

上記の例では、変数`str`に"Hello World!"という文字列を代入し、その中からパターンにマッチする文字"l"を削除しています。削除する際には、変数名の前に"//"をつける必要があります。そして、新しい変数`new_str`に削除後の文字列を代入します。最後に、新しい変数の中身を出力することで、削除が正常に行われたかを確認できます。

## ディープダイブ

上記のコードでは、パターンにマッチする文字を一度にすべて削除していますが、場合によっては一部の文字のみを削除したい場合もあります。そのような場合は、下記のようにコードを書くことができます。

```Bash
# 文字列を変数に代入
str="Hello World!"

# パターンにマッチする文字を削除
new_str=${str//l/}

# パターンにマッチする最初の文字を削除
new_str=${new_str/#H/}

# パターンにマッチする最後の文字を削除
new_str=${new_str/%!/}

# 出力
echo $new_str

# Output: Heo Word
```

上記の例では、先ほどと同じく"Hello World!"という文字列からlを削除し、その後に最初の文字Hと最後の文字!を削除しています。削除したい位置によって、`/#`や`/%`という記号を使い分けることができます。

## 参考リンク

- [Bash Guide for Beginners - Variable Substitution](http://mywiki.wooledge.org/BashGuide/Substitution)
- [Bash Hackers Wiki - Parameter Expansion](http://wiki.bash-hackers.org/syntax/pe)
- [GNU Bash Manual - Parameter Expansion](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)

---

## 関連リンク

- [Bashコマンドの基本と基礎](https://www.oracle.com/technetwork/jp/articles/opensource/Linux-Bash2-1506041-ja.html)
- [Bash Tips: 様々なパターンを削除する](https://qiita.com/muranet/items/3faaf402a6a483c03984)
- [初心者が迷わないためのBash入門ガイド](https://qiita.com/meznat/items/08a4a0155e5869bbcc78)