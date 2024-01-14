---
title:                "Fish Shell: 正規表現の使用方法"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜ？

正規表現を使用する理由は、文字列内で特定のパターンを検索や抽出する必要がある場合に非常に便利です。例えば、電話番号やメールアドレスのような特定のフォーマットに従う文字列を検索する時に、正規表現を使用することでより簡単に見つけることができます。

## 使い方

正規表現は、Fish Shellの「grep」ツールを使用して簡単に実行することができます。例えば、電話番号を含む文字列を検索する場合、次のようなコマンドを実行します。

```Fish Shell
grep -o "[0-9]{3}-[0-9]{4}-[0-9]{4}" text.txt
```

上記のコマンドでは、ファイル「text.txt」内で「000-0000-0000」の形式に従う電話番号を検索し、それを表示します。

## 深堀り

正規表現をより詳しく学ぶには、様々なパターンを使用して実際にコードを書いてみるようにしましょう。例えば、文字列内で特定の文字「abc」の前にある文字を抽出したい場合、次のようなコマンドを使用できます。

```Fish Shell
grep -o ".abc" text.txt
```

ここで使用されている「.」は任意の文字を表し、「abc」は特定の文字列を表します。これにより、文字列内にある「abc」の前にある文字が抽出されます。

## 参考リンク

[Fish Shellの正規表現についてのドキュメント](https://fishshell.com/docs/current/cmds/grep.html)

[正規表現を使ったパターンマッチングのチュートリアル](https://qiita.com/horikeso/items/e5c6d53f4cbb11d52732)