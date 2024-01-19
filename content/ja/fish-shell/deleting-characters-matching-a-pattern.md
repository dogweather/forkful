---
title:                "パターンに一致する文字を削除する"
html_title:           "C: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 何となぜ？

パターンに一致する文字を削除するとは、文字列内の特定のパターンの文字を取り除くプログラミング手法を指します。プログラマーはこの手法を用いて、不要な文字を排除し、データの整形やパースを行います。

## 使い方：

Fish Shellにおいて、パターンに合致する文字を削除する方法を示します。

```Fish Shell
set var "Hello, World!"
echo $var | string replace -r "World" ""
```

出力結果：

```Fish Shell
Hello,
```

上記のコードでは、"World"という文字列を空（""）に置き換えて削除しています。

## より深く：

この手法の歴史的な文脈、代替策、実装の詳細について掘り下げます。

1. **歴史的文脈**：Fish Shellは2005年に初リリースされた、ユーザビリティに重点を置いたUnixシェルです。文字列操作はその重要な部分を占め、パターンマッチングの削除はその一部となります。

2. **代替方法**：`sed`コマンドもパターンに一致する文字を削除するために利用できます。

    ```Fish Shell
    echo $var | sed 's/World//g'
    ```

3. **実装の詳細**：Fish Shellでは`string replace`コマンドを使用して、パターンに一致する文字を削除します。`-r`オプションは正規表現の使用を意味します。

## 関連リンク：

1. [Fish Shell公式ウェブサイト](https://fishshell.com/)
2. [Fish ShellのGitHubリポジトリ](https://github.com/fish-shell/fish-shell)
3. [Fish Shellのstring replaceの具体的な使い方](https://fishshell.com/docs/current/cmds/string-replace.html)