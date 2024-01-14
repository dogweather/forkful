---
title:    "PHP: 「テキストファイルの読み込み」"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## なぜ読む必要があるのか？

テキストファイルを読むことは、PHPプログラミングにおいて非常に重要です。たとえば、データベースに保存された情報を取得したい場合や、外部からのデータを読み込みたい場合など、さまざまなシナリオでテキストファイルを扱う必要があります。この記事では、テキストファイルを読み込む方法をご紹介します。

## 読み込みの方法

テキストファイルを読み込むには、PHPの組み込み関数である`file()`を使用します。この関数は、指定されたテキストファイルを配列として返します。例えば、以下のコードを使用してテキストファイルを読み込むことができます。

```PHP
$file = file('sample.txt'); // "sample.txt"は読み込むテキストファイルのパス
```

次に、`foreach`ループを使用して取得したファイルの配列をループ処理します。

```PHP
foreach ($file as $line) {
    // 各行の処理
}
```

また、`file_get_contents()`を使用してテキストファイルの中身を一括で取得することもできます。この関数は、指定されたファイルを文字列として返します。

```PHP
$content = file_get_contents('sample.txt'); // "sample.txt"は読み込むテキストファイルのパス
```

## 深堀り

テキストファイルを読み込む際には、`file()`や`file_get_contents()`以外にも`fopen()`と`fgets()`を使用することもできます。`fopen()`は、ファイルを開き、ファイルポインタを返します。`fgets()`は、ファイルポインタから一行を読み込みます。以下のように使用します。

```PHP
$handle = fopen('sample.txt', 'r'); // "sample.txt"は読み込むテキストファイルのパス
if ($handle) {
    while (($line = fgets($handle)) !== false) {
        // 各行の処理
    }
    fclose($handle);
}
```

さらに、テキストファイルを書き出す際には`fwrite()`を使用します。この関数は、指定したファイルに指定した文字列を書き込みます。また、ファイルを閉じる際には`fclose()`を使用します。

```PHP
$handle = fopen('output.txt', 'w'); // "output.txt"は書き出すテキストファイルのパス
fwrite($handle, '書き込む文字列');
fclose($handle);
```

## おわりに

この記事では、PHPでテキストファイルを読み込む方法について学びました。ファイルを読み込む際には、`file()`や`file_get_contents()`、`fopen()`と`fgets()`を使用することができます。また、テキストファイルを書き出す際には、`fwrite()`を使用します。これらの関数を上手に使いこなして、様々なシーンでテキストファイルを扱えるようになりましょう。

## 関連リンク

- [PHP: file - Manual](https://www.php.net/manual/ja/function.file.php)
- [PHP: file\_get\_contents - Manual](https://www.php.net/manual/ja/function.file-get-contents.php)
- [PHP: fopen - Manual](https://www.php.net/manual/ja/function.fopen.php)
- [PHP: fgets