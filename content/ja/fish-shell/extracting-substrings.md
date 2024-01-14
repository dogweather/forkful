---
title:    "Fish Shell: サブストリングの抽出"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## なぜ

サブストリングを抽出することのメリットは、文字列から特定の部分を取り出し、必要な情報だけを抽出できることです。これはテキスト処理やデータ解析などの多くのタスクに役立ちます。

## 方法

Fish Shellでサブストリングを抽出するには、`string`コマンドを使用します。例えば、次のコマンドを実行すると、文字列の3番目の文字から5番目の文字までが抽出されます。

```Fish Shell
string sub 3 5 "abcdefg"
# 出力: cde
```

また、特定の文字列を指定してサブストリングを抽出することもできます。例えば、次のコマンドを実行すると、文字列の中から"world"という部分文字列が抽出されます。

```Fish Shell
string match "world" "Hello world!"
# 出力: world
```

さらに、配列内の特定の要素だけを抽出して新しい配列を作成することもできます。次のコマンドでは、配列`fruits`から2つの要素を抽出し、新しい配列`selected_fruits`を作成しています。

```Fish Shell
set fruits apple banana orange lemon
set selected_fruits (string sub 2 4 $fruits)
# 出力: [banana orange]
```

## ディープダイブ

サブストリングを抽出するには、`string`コマンドの他にも`split`コマンドや`substr`コマンドなどがあります。これらのコマンドを組み合わせることで、より高度なサブストリングの抽出が可能になります。

また、コマンドライン引数や環境変数からの文字列の抽出にも`string`コマンドを使用することができます。これにより、シェルスクリプト内で柔軟に文字列を操作することができます。

## 参考リンク

- [Fish Shellドキュメント(英語)](https://fishshell.com/docs/current/)
- [Fish Shellチートシート(英語)](https://devhints.io/fish)
- [Fish Shellユーザーガイド(日本語)](https://munisystem.github.io/docs-fish-shell/)
- [Fish Shell公式GitHubリポジトリ(英語)](https://github.com/fish-shell/fish-shell)