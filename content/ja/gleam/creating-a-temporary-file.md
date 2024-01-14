---
title:                "Gleam: 一時ファイルの作成"
simple_title:         "一時ファイルの作成"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

#なぜ

Gleamで一時ファイルを作成するのには、様々な理由があります。例えば、効率的なデータ処理を行うためには一時ファイルを使用する必要があるかもしれません。また、一時的に保存する必要のあるデータや、一時的なバックアップファイルを作成する場合にも、一時ファイルは便利です。

#作り方

一時ファイルを作成するためには、Gleamの標準モジュールであるFile.Tempモジュールを使います。まず、モジュールをインポートします。

```Gleam
import File.Temp
```

次に、`File.Temp.create/2`関数を使って一時ファイルを作成します。この関数には、作成するファイルの名前と拡張子を指定します。

```Gleam
let { temp_file, temp_file_name } = File.Temp.create("example", ".txt")
```

上記の例では、`example.txt`という名前の一時ファイルが作成されます。また、関数は作成した一時ファイルのパスを返し、それを`temp_file`変数に、ファイル名だけを`temp_file_name`変数に代入しています。

#深堀り

一時ファイルを作成する際に、Gleamではどのような処理が行われているかを見てみましょう。`File.Temp.create/2`関数は、内部で`File.Temp.random_filename/1`関数を使ってランダムなファイル名を生成し、作成する一時ファイルのパスを決定します。また、GleamのFileモジュールを使って実際にファイルを作成しています。

#参考リンク

- [Gleam File.Tempドキュメンテーション](https://gleam.run/documentation/standard-library/file#temp-module)
- [Gleamファイルモジュールドキュメンテーション](https://gleam.run/documentation/standard-library/file)