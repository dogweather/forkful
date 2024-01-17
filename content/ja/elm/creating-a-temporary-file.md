---
title:                "「一時ファイルの作成」"
html_title:           "Elm: 「一時ファイルの作成」"
simple_title:         "「一時ファイルの作成」"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 何&なぜ?

一時ファイルを作成するとは、一時的な情報やデータを保存するために作成されるファイルのことです。プログラマーは、プログラム実行中に一時的なデータを保存するために一時ファイルを作成します。

## 方法:

```
Elm.File.temp 
```

この関数を使用すると、一時ファイルを作成することができます。

```
Elm.File.temp 
```

## 深堀り:

一時ファイルの歴史的な文脈は、パソコンやインターネットの登場とともに始まりました。以前は、プログラムを実行する際には、すべてのデータをコンピューターのメモリに保存する必要がありました。しかし、メモリは限られており、大量のデータを保存することができませんでした。そのため、一時ファイルの作成が必要となりました。

一時ファイルを作成する方法としては、他にもオンメモリデータベースやメモリマップドファイルなどの方法がありますが、一時ファイルはデータの一時的な保存に便利です。一時ファイルの実装にはさまざまな方法がありますが、その基本的な考え方は同じです。

## 関連リンク:

- オンメモリデータベース: https://www.ipsj.or.jp/award/6faeag000000ey4t-att/06-02.pdf
- メモリマップドファイル: http://www.csclsp2017.org/wp/wp-content/uploads/2017/01/6-2-Shudo.pdf