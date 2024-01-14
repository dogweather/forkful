---
title:    "PHP: テキストファイルの読み込み"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ

テキストファイルを読むことの重要性は、プログラミングにおいて非常に重要です。テキストファイルを読むことにより、データの保存、表示、および編集が容易になります。また、多くのプログラミング言語では、テキストファイルの読み取りが基本的な機能となっています。そのため、テキストファイルを読む方法を学ぶことは、プログラミングにおいて基本的なスキルの一つと言えます。

## 方法

```PHP
$file = fopen("sample.txt", "r"); // ファイルを読み込む
if (!$file) { // ファイルがない場合、エラーを出力
  die("ファイルが見つかりません");
}
while (!feof($file)) { // ファイルの終端に達するまでループ
  echo fgets($file) . "<br>"; // ファイルから一行読み込み、改行を付けて出力
}
fclose($file); // ファイルを閉じる
```

上記のコードを実行すると、指定したテキストファイルの内容がページに表示されます。`fopen()`関数を使用してファイルを開き、`feof()`関数でファイルの終端に達するまでループさせ、`fgets()`関数で一行ずつ読み込み、`fclose()`関数でファイルを閉じるという基本的な流れになります。

## 深堀り

テキストファイルを読み込むには、`fopen()`を始めとするいくつかの関数を使用します。それぞれの関数には、オプションとしてファイルを読み込むモードや、ファイルポインタを指定するパラメータが存在します。また、テキストファイルの読み込みだけでなく、書き込みや編集も可能です。さらに、異なるフォーマットのテキストファイルを読み込む方法や、文字コードの変換も可能です。

## 参考リンク

- [PHPマニュアル - ファイルの読み込み](https://www.php.net/manual/ja/function.fopen.php)
- [TechAcademyマガジン - PHPファイル入出力の基本（fopenメソッド）](https://techacademy.jp/magazine/459)
- [PHP入門編 - ファイルへのアクセス](https://www.javadrive.jp/php/file/index1.html)

## 参考文献

- [PHPでファイルを読み込む方法](https://laboradian.com/fileopen/)
- [PHPテキストファイルの読み込みと文字コード変換の方法](https://huolong2001.com/blog/2019/08/23/1084/)