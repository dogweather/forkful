---
title:                "「JSONで作業する」"
html_title:           "PHP: 「JSONで作業する」"
simple_title:         "「JSONで作業する」"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/working-with-json.md"
---

{{< edit_this_page >}}

## 何 & なぜ？

JSONを扱うこととは、データのやりとりをする際に使用する形式の1つです。プログラマーたちはJSONを使用する理由として、データを効率的に取り扱い、他の形式よりも簡単に使えることが挙げられます。

## 方法：

PHPでJSONを扱う方法は簡単です。まず、JSONをパースするために利用できる「json_decode」関数を使います。例えば、以下のようにコードを書くことができます。

```PHP
<?php
$json = '{"name":"John", "age":30, "city":"New York"}';
$decoded = json_decode($json);
echo $decoded->name; // John
echo $decoded->age; // 30
echo $decoded->city; // New York
?>
```

```json_decode```関数はJSONデータをオブジェクト型に変換し、指定したプロパティを取得することができます。

もしJSONデータを配列型で取得したい場合は、```json_decode($json, true)```というように引数に```true```を指定します。

例えば、以下のようにコードを書くことができます。

```PHP
<?php
$json = '{"name":"John", "age":30, "city":"New York"}';
$decoded = json_decode($json, true);
echo $decoded["name"]; // John
echo $decoded["age"]; // 30
echo $decoded["city"]; // New York
?>
```

## 深堀り：

JSONはJavaScript Object Notationの略であり、JavaScriptでデータを格納するための軽量なデータフォーマットです。1999年にJavaScriptの開発者によって作成されましたが、現在では広く使われるようになっています。

JSONの代替としては、XMLやYAMLなどのデータフォーマットがありますが、JSONのシンプルさと扱いやすさから、プログラマーたちの間で幅広く使用されています。

また、PHPに限らず、HTMLやJavaScriptなど他のプログラミング言語でも同様にJSONを扱うことができます。

## 関連情報：

- [PHP公式ドキュメント：json_decode](https://www.php.net/manual/ja/function.json-decode.php)
- [JSONの歴史について知る](https://ja.wikipedia.org/wiki/JavaScript_Object_Notation)
- [JSON以外のデータフォーマットについて学ぶ](https://dev.classmethod.jp/server-side/php/vs-json-and-others/)
- [無料で学べる最高のPHP講座](https://h-navi.jp/php-school/)