---
title:    "Elixir: 一時ファイルを作成する"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# なぜ
あなたは一時ファイルを作成するのでしょうか？一時ファイルを作成することにはどのようなメリットがあるのでしょうか？

## 作り方
一時ファイルを作成するには、以下のようなElixirのコードを使用します。

```
File.temp_file!("temp_dir", "prefix")
```

これは、temp_dirディレクトリにprefixを付けて一時ファイルを作成するコマンドです。また、以下のようなオプションを追加することもできます。

```
File.temp_file!("temp_dir", "prefix", encoding: :utf8, suffix: ".txt")
```

これにより、一時ファイルのエンコーディングをUTF-8に設定し、拡張子を.txtに指定することができます。

## ディープダイブ
一時ファイルを作成するとき、大切なことはファイルを使用した後に削除することです。これは、一時ファイルが使用されなくなった場合でも、システム上にそのまま残ってしまう可能性があるからです。

Elixirでは、以下のようなコードを使用して一時ファイルを削除することができます。

```
File.rm(file)
```

## 参考サイト
- [Elixir Documentation](https://elixir-lang.org/getting-started/file.html#temporary-files)
- [Elixir School](https://elixirschool.com/ja/lessons/advanced/files/#temporary-files)

---
# 参考リンク