---
title:    "Gleam: ディレクトリが存在するかどうかをチェックする"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ
ディレクトリが存在するかどうかを確認することの利点は、コードのロジックを追加する前に、特定の処理が必要かどうかを判断することができることです。

## 方法
```Gleam
// `dir_exists`関数を使用してディレクトリが存在するかどうかを確認する方法
dir_exists("my_directory");

// 出力：trueまたはfalseを返します
```

```Gleam
// `dir_exists_or_create`関数を使用して、ディレクトリが存在しない場合はディレクトリを作成する方法
dir_exists_or_create("my_directory");

// 出力：trueまたはfalseを返します
```

```Gleam
// `dir_exists_with_permissions`関数を使用して、指定したパーミッションでディレクトリが存在するかどうかを確認する方法
dir_exists_with_permissions("my_directory", [read: true, write: true, execute: true]);

// 出力：trueまたはfalseを返します
 ```

## 深堀り
ディレクトリが存在するかどうかを確認する際には、ファイルシステムのパーミッションを考慮することも重要です。また、プログラムによっては自動的にディレクトリを作成する必要があるため、`dir_exists_or_create`関数の使用も検討してください。

## 参考リンク
- [Gleam公式ドキュメント](https://gleam.run/documentation/)：Gleamの公式ドキュメント
- [ディレクトリの存在を確認する方法](https://www.digitalocean.com/community/tutorials/how-to-check-if-a-directory-exists-in-a-bash-shellscript)：Bashシェルスクリプトでディレクトリの存在を確認する方法の例