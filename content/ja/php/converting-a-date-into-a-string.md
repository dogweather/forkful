---
title:                "PHP: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

「日付を文字列に変換する」の理由について：日付を文字列に変換するメソッドを使うことで、データベースやファイルなどで扱いやすい形式にデータを整えることができます。

## 日付を文字列に変換する方法

```PHP
// 日付を文字列に変換する
$date = date('Y-m-d', strtotime('2020-01-01'));
echo $date; // 出力: 2020-01-01

// 自分でフォーマットを指定する
$date = date('m/d/Y', strtotime('2020-01-01'));
echo $date; // 出力: 01/01/2020
```

## 日付を文字列に変換する深い情報

日付を文字列に変換する際には、自分でフォーマットを指定することもできます。また、`strtotime()`関数を使うことで、指定した日付をタイムスタンプに変換できます。さらに、フォーマットに使用するパラメータを調べる際には、PHPの公式ドキュメントを参考にすることをおすすめします。

## 詳しく知りたい方へ

PHPの公式ドキュメントを参考に、さまざまなフォーマットのパラメータについて学ぶことができます。

## 関連リンク

- [PHPの公式ドキュメント：日付と時刻のフォーマット](https://www.php.net/manual/ja/function.date.php)
- [PHPの公式ドキュメント：strtotime()関数](https://www.php.net/manual/ja/function.strtotime.php)