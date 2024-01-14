---
title:    "Gleam: 一時ファイルの作成"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## なぜ
何らかの理由で一時ファイルが必要になることがあります。そのような場合、Gleamプログラミング言語を使用して一時ファイルを作成することができます。

## 作り方
まず、gleam_temporary_fileパッケージをインポートします。次に、creation_time関数を使用して一時ファイルを作成し、path関数を使用してファイルへのパスを取得します。最後に、必要な処理が完了したら、delete関数を使用してファイルを削除します。

```Gleam
import gleam_temporary_file

// 一時ファイルを作成
let temp_file = gleam_temporary_file.creation_time()

// パスを取得
let file_path = gleam_temporary_file.path(temp_file)

// ファイルの操作
// ここでは、ファイルにデータを書き込む例を示します
file_path = "Hello, world!"
```

上記のコードでは、gleam_temporary_fileパッケージのcreation_time関数が現在のタイムスタンプを使用して一時ファイルを作成し、path関数がそのパスを返します。その後、ファイルに書き込むことができます。

## ディープダイブ
gleam_temporary_fileパッケージは、一時ファイルを作成するための便利な関数を提供しています。また、一時ファイルを作成する際の参照やエラー処理についても詳しく説明されています。詳細については、[公式ドキュメント](https://gleam.run/packages/gleam_temporary_file/latest/)を参考にしてください。

## 参考
[gleam_temporary_fileパッケージ](https://github.com/gleam-lang/gleam_temporary_file)
[公式ドキュメント](https://gleam.run/packages/gleam_temporary_file/latest/)