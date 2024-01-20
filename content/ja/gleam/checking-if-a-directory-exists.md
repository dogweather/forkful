---
title:                "ディレクトリが存在するかどうかの確認"
html_title:           "Gleam: ディレクトリが存在するかどうかの確認"
simple_title:         "ディレクトリが存在するかどうかの確認"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 何とどうして？ | What & Why?
ディレクトリが存在するかどうかの確認は、ファイルシステムへのデータの保存分析に不可欠です。存在しないディレクトリに書き込もうとするとエラーが発生しますから、プログラマーはあらかじめチェックを行います。

## 実施方法 | How to:
以下は、Gleamでディレクトリ存在の確認を行う方法を示すコードです。

```Gleam
import gleam/otp/process
import gleam/otp/process.{ExitStatus}
import gleam/io.{Result}
import gleam/string
​

let exist_directory = fn(directory: String) {
let command = string.concat(["test -d ", directory, " && echo 'exists' || echo 'does not exist'"])
Result.unwrap(process.run(command, []))
}

match exist_directory("path-to-check") {
 Ok(status) -> 
  case status {
   Success(_("exists")) -> print("Directory exists.")
   Success(_("does not exist")) -> print("Directory does not exist.")
   Success(_) | Error(_) -> print("An unknown error occurred.")
  }
Error(error) -> io.println(error)
}  
```
このコマンドは、特定のディレクトリが存在するかどうかをテストします。存在する場合は"exists"を、存在しない場合は"does not exist"を出力します。

## 深掘り | Deep Dive
ディレクトリ存在チェックの手法は時代と共に進化してきました。初期のUNIXシステムでは、コマンド`test -d <directory>`を利用していました。最近では、多くのプログラミング言語が独自の抽象化を提供しています。

代替案としては、`os.path.exists()`や`os.path.isdir()`等の専用関数を使用する方法もあります。

Gleamでは存在チェックはシェルコマンドを呼び出し実行しますが、これは他の言語やフレームワークで一般的に利用される方法とは異なります。これはGleamがErlangのOTPを活用しており、目的のシステムコールへの直接アクセスを提供していないためです。

## 参照 | See Also
以下のリンクで、より深くGleamの説明、さらに詳細な情報が見られます：

- [公式Gleamドキュメンテーション](https://gleam.run/docs/)
- [fs module in Node.js](https://nodejs.org/api/fs.html#fs_fs_exists_path_callback)
- [Pythonのos library](https://docs.python.org/3/library/os.path.html)