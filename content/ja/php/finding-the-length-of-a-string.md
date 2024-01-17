---
title:                "文字列の長さを見つける"
html_title:           "PHP: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 何故やるのか？
文字列の長さを見つけることは、プログラマーにとって重要なタスクです。文字列の長さを見つけることにより、必要なデータを正確に処理することができます。例えば、ユーザーが入力したパスワードが指定された文字数を満たしているかを確認する際に使用します。

## 方法：
```PHP
// 文字列の長さを表示する例
echo strlen("こんにちは！"); // 出力：6
```
```PHP
// マルチバイト文字列の長さを表示する例
echo mb_strlen("こんにちは！", "UTF-8"); // 出力：6
```

## 深堀り
(1) 歴史的な文脈：文字列の長さを取得する機能は、1980年代にC言語で最初に実装されました。その後、多くのプログラミング言語でも採用されました。
(2) 代替案：文法に依存しない方法として、mb_strlen()関数を使用することもできます。これにより、マルチバイト文字列の長さをより正確に測定することができます。
(3) 実装の詳細：文字列の長さを取得する方法は、プログラミング言語によって異なります。一般的な方法は、文字列の長さを保存する変数を使用することです。また、メモリ上に文字列の長さを示すバイト数が格納されている場合もあります。

## 参考リンク：
- [PHP公式ドキュメント – strlen()関数](https://www.php.net/manual/ja/function.strlen.php)
- [PHP公式ドキュメント – mb_strlen()関数](https://www.php.net/manual/ja/function.mb-strlen.php)
- [C言語のstrlen()関数の由来](https://stackoverflow.com/questions/20229361/why-was-the-strlen-function-called-strlen)