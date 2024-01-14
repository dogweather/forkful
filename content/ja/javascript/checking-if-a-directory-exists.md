---
title:                "Javascript: ディレクトリが存在するかを確認する"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

＃＃なぜ

ディレクトリが存在するかどうかを確認する理由は、コーディングの効率性を高めるためです。特定のフォルダーが必要な場合、そのフォルダーが存在するかどうかをプログラムで確認することで、ユーザーが不必要なエラーやタイムアウトを回避することができます。

＃＃方法

JavaScriptを使用してディレクトリが存在するかどうかを確認するためには、`fs`モジュールを使用します。次のコードは、`checkDirectory()`関数を定義し、指定されたディレクトリが存在するかどうかをチェックします。 

```Javascript
const fs = require('fs');

function checkDirectory(directory) {
  try {
    // 引数として渡されたディレクトリのパスを確認
    fs.statSync(directory);
    console.log("指定されたディレクトリは存在します。");
  } catch (err) {
    if (err.code === 'ENOENT') {
      console.log("指定されたディレクトリは存在しません。");
    }
  }
}

// ディレクトリのパスを引数として渡します。この例では、documentsフォルダーを指定します。
checkDirectory('C:/Users/user/Documents');
```

上記のコードを実行すると、指定したディレクトリが存在するかどうかに応じて、適切なメッセージが表示されます。たとえば、指定したパスが存在する場合は、次のように表示されます。

```
指定されたディレクトリは存在します。
```

しかし、ディレクトリが存在しない場合は、次のように表示されます。

```
指定されたディレクトリは存在しません。
```

＃＃深堀り

ディレクトリが存在するかどうかを確認するには、`fs`モジュールの`statSync()`メソッドを使用します。これは同期的なメソッドであり、指定されたパスのファイルまたはディレクトリの情報を返します。ファイルまたはディレクトリが存在する場合は、正常な返り値が返されます。しかし、存在しない場合は例外がスローされます。

`statSync()`メソッドを使用する代わりに、コールバック関数を使用して非同期的に`stat()`メソッドを呼び出すこともできます。

```
fs.stat(directory, (err, stats) => {
  if (err) {
    // ファイルまたはディレクトリが存在しない場合
  } else {
    // ファイルまたはディレクトリが存在する場合
  }
})
```

また、`fs`モジュールには、存在するかどうかを確認するための他のメソッドや関数もあります。詳細は[公式ドキュメント](https://nodejs.org/api/fs.html#fs_fs_stat_path_options_callback)を参照してください。

＃＃見てみる

- [fsモジュールの公式ドキュメント](https://nodejs.org/api/fs.html)
- [JavaScriptでディレクトリの存在を確認する方法](https://qiita.com/y-style/items/3570046864f468a4fc8e) (日本語)