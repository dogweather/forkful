---
title:    "PHP: テキストファイルの書き方"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## なぜ

テキストファイルを書く理由は、データを簡単に保存し、検索することができるからです。

## 方法

テキストファイルを書くには、PHPの`fwrite()`関数を使用します。以下がサンプルコードと出力例です。

```PHP
<?php
// テキストファイルを書き込みモードで開く
$fp = fopen('sample.txt', 'w');
// テキストを書き込む
fwrite($fp, 'Hello World!');
// ファイルを閉じる
fclose($fp);
?>
```

**出力例**

sample.txtファイルには以下のように書き込まれます。

```
Hello World!
```

## 深堀り

テキストファイルを書くには、使用するファイルのパスを指定し、`fwrite()`関数を使用してテキストを書き込みます。さらに、`fopen()`関数のモードを「w」にすることで、ファイルが存在しない場合でも新しいファイルを作成することができます。

また、テキストファイルを読み込む際には`fread()`関数を使用します。詳細な使用方法やパラメータについては、PHPの公式ドキュメントを参照してください。

## 参考リンク

- [PHP公式ドキュメント：fwrite関数](https://www.php.net/manual/ja/function.fwrite.php)
- [PHP公式ドキュメント：fread関数](https://www.php.net/manual/ja/function.fread.php)