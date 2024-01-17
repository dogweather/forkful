---
title:                "ディレクトリが存在するかどうかをチェックする"
html_title:           "TypeScript: ディレクトリが存在するかどうかをチェックする"
simple_title:         "ディレクトリが存在するかどうかをチェックする"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 何となぜ？

ディレクトリが存在するかどうかを確認することは、プログラマーにとって重要です。これにより、ファイルを探したり、特定の操作を行う前にフォルダーを作成したりすることができます。

## 方法：

### TypeScriptを使用する場合

```TypeScript
if (fs.existsSync(path)) {
    console.log("ディレクトリが存在します");
} else {
    console.log("ディレクトリが存在しません");
}
```

#### 出力：

```
ディレクトリが存在します
```

### Node.jsを使用する場合

```TypeScript
fs.access(path, (err) => {
    if (err) {
        console.log("ディレクトリは存在しません");
    } else {
        console.log("ディレクトリが存在します");
    }
});
```

#### 出力：

```
ディレクトリが存在します
```

## ディープダイブ：

### 歴史的な背景

ディレクトリの存在をチェックする方法は様々ありますが、古くから存在する方法としては、ファイルまたはディレクトリのパスを指定して`access()`関数を使う方法があります。

### 代替手段

ディレクトリが存在するかどうかを確認するもう一つの方法としては、ディレクトリを開こうとしてエラーが発生した場合にそのディレクトリが存在しないということを示すことができます。

### 実装の詳細

Node.jsでは、`fs`モジュールを使用してディレクトリの存在を確認することができます。`fs`モジュールは、ファイルシステムにアクセスするためのさまざまなメソッドを提供しています。`exists()`関数は、指定されたファイルまたはディレクトリが存在するかどうかを確認します。

## 関連リンク：

- [Node.jsのfsモジュール](https://nodejs.org/api/fs.html)
- [fs.existsSync()のドキュメント](https://nodejs.org/api/fs.html#fs_fs_existssync_path)
- [Node.jsのコアライブラリを使用したディレクトリの存在確認方法](https://nodejs.org/api/fs.html#fs_fs_exists_path_callback)