---
title:    "Fish Shell: 文字列の長さを見つける"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列の長さを調べる理由は、プログラミングにおいて非常に重要なスキルであるためです。文字列の長さを正確に知ることで、コードを効率的かつ正確に書くことができます。

## 方法

文字列の長さを調べるには、Fish Shellの組み込みの `string length` コマンドを使用します。以下のコマンドを実行すると、指定した文字列の長さが表示されます。

```fish shell
string length "Hello, world!"
```

出力：

```fish shell
13
```

もちろん、変数に代入して使用することもできます。

```fish shell
set text "This is a sample text."
string length $text
```

出力：

```fish shell
23
```

また、`string length` コマンドのオプションを使用することで、指定した文字列の長さをバイト単位ではなく文字単位でカウントすることもできます。詳細については、`man string` コマンドを参照してください。

## ディープダイブ

`string length` コマンドは、文字列の長さを調べるだけでなく、その他の文字列操作にも使用することができます。例えば、 `string length` コマンドを Pipelinesと組み合わせることで、特定の文字数を超える文字列をフィルタリングすることができます。

```fish shell
ls -1 | grep txt | string length | grep -v 10 # 「txt」が含まれるファイル名を除外
```

このように、`string length` コマンドは文字列の長さを調べるだけでなく、より複雑なタスクにも活用することができます。

## もっと詳しく知りたい方へ

- [Fish Shellのガイド](https://fishshell.com/docs/current/index.html)
- [Fish Shellの組み込みコマンド](https://fishshell.com/docs/current/cmds.html)
- [Manコマンドの使い方](https://qiita.com/masaru/items/3ae1f9335bf2203b5953)

## 関連リンク

- [Fish Shell公式サイト](https://fishshell.com/)
- [Fish ShellのGitHubリポジトリ](https://github.com/fish-shell/fish-shell)
- [Fish Shellコミュニティ](https://gitter.im/fish-shell/fish-shell)