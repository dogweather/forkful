---
title:                "Fish Shell: 「サブストリングを抽出する」"
simple_title:         "「サブストリングを抽出する」"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ?

Substringの抽出に従事する理由は何でしょうか？Fish Shellプログラミングにおいて、特定の部分のみを抽出することはとても便利です。例えば、テキストから特定の単語を見つけ出したり、文字列を加工したりする際によく使われます。是非試してみてください！

## 方法

抽出する方法はとても簡単です。まずは文字列を変数に設定し、その後にsubstringコマンドを使用します。下記のコードを参考にしてみてください。

```Fish Shell
set text "こんにちは、私はFish Shellプログラマーです！"
substring な私はFish Shellプログラマーです！」
```

抽出した結果は次のようになります。

```
私はFish Shellプログラマーです！
```

## 深堀り

substringコマンドには様々なオプションがあります。例えば、インデックス番号を指定して抽出する部分の範囲を指定することもできます。詳しくはFish Shellの公式ドキュメントを参考にしてください。

## その他のリンク

[Fish Shell公式ドキュメント](https://fishshell.com/docs/current/index.html)

## 参考

[Substringの抽出方法について](https://fishshell.com/docs/current/cmds/substr.html)