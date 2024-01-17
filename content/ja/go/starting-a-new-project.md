---
title:                "新しいプロジェクトを開始する"
html_title:           "Go: 新しいプロジェクトを開始する"
simple_title:         "新しいプロジェクトを開始する"
programming_language: "Go"
category:             "Go"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/starting-a-new-project.md"
---

{{< edit_this_page >}}

「What & Why?」
プログラマーが新しいプロジェクトを始めるとは、新しいソフトウェアやアプリケーションを作成することを指します。プログラマーが新しいプロジェクトを始める理由は、新しいアイデアを実現するためや、既存のコードを改善するためなど、様々あるでしょう。

「How to:」
`` `Go ... ` ``コードブロック内に、新しいプロジェクトを始めるためのコーディング例とサンプルの出力を記載しました。以下のステップに従うことで、新しいプロジェクトを始めることができます。

```
// 新しいプロジェクトを作成するためのコマンド
Go mod init example.com/myproject
```

```
// サンプルの出力
"Initialized module and created go.mod in /Users/example/myproject"
```

「Deep Dive」
新しいプロジェクトを始める方法として、`` `Go ... ` ``コードブロックに示した``Go mod init``コマンドを使用することができます。しかし、このコマンドはGo 1.11以降で使用可能であり、それ以前のバージョンを使用している場合は、代わりに``go get``コマンドを使用する必要があります。

また、新しいプロジェクトを始めるための代替手段として、既存のプロジェクトをフォークする方法が挙げられます。これにより、コードベースを再利用することができ、開発をより迅速に進めることができます。

「See Also」
関連情報には以下のリンクを参考にしてください。

- https://golang.org/cmd/go/#hdr-Create_a_new_module_in_the_current_directory
- https://golang.org/doc/go1.11#modules