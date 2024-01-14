---
title:    "Gleam: 一時ファイルの作成"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## なぜ

一時ファイルを作成する理由は、データの一時的な保存や処理に便利であるためです。

## 作り方

```Gleam
// 一時ファイルの作成
let temp_file = File.temp() 
// 作成された一時ファイルのパスを取得
let temp_path = temp_file.path() 
// 一時ファイルにデータを書き込む
let _ = File.write(temp_path, "Hello, world!") 
// 一時ファイルからデータを読み込んで表示
let content = File.read(temp_path) 
IO.print(content) // => Hello, world!
```

## ディープダイブ

一時ファイルの作成には、一時ファイルを作成するための関数や一時ファイルのパスなど、さまざまなオプションがあります。詳しくは [Gleamのドキュメント](https://gleam.run/documentation/) をご覧ください。

## 参考リンク

- [Gleamのドキュメント](https://gleam.run/documentation/)
- [Fileモジュールのドキュメント](https://gleam.run/documentation/#file)
- [一時ファイルの作成について](https://en.wikipedia.org/wiki/Temporary_file)