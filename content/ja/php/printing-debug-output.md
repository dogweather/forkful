---
title:    "PHP: デバッグ出力のプリント"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## なぜデバッグ出力を表示するのか

プログラミングを行う際、正しいコードを書くことはとても重要です。しかし、デバッグ出力を表示することは、コードをより理解し、エラーをより早く特定するために役立ちます。

## 方法

デバッグ出力を表示するには、PHPの「```echo```」コマンドを使用します。例えば、以下のコードを使用すると、変数の値を表示することができます。

```PHP
$name = "太郎";
echo $name;
```

出力:

```
太郎
```

変数の値だけでなく、条件文やループなどの特定のコードブロックの結果も表示することができます。例えば、以下のコードを使用すると、ループ内での変数の値を表示することができます。

```PHP
for ($i = 1; $i <= 5; $i++) {
  echo $i;
}
```

出力:

```
12345
```

## ディープダイブ

デバッグ出力を使用することで、コードの実行中に起きる可能性のあるエラーやバグをより早く発見することができます。また、コードのどの部分が実行されているかを確認することもできます。これにより、デバッグおよび修正の時間を大幅に短縮することができます。

## その他参考リンク

- [PHP公式ドキュメント](https://www.php.net/manual/ja/function.echo.php)
- [PHPのデバッグ方法](https://www.php-factory.net/php-debug.php)
- [PHPの記事「デバッグ：ソースコードを修正するためのテクニック」](https://www.webprofessional.jp/debugging-source-code-tips/)