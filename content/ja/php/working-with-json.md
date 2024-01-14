---
title:                "PHP: 「jsonを使ったコンピュータプログラミング」"
simple_title:         "「jsonを使ったコンピュータプログラミング」"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/working-with-json.md"
---

{{< edit_this_page >}}

## なぜ
JSONを扱うことに取り組む理由については、次のようなものが考えられます：

- データの収集や処理において、より軽量で柔軟性が高く、Web開発においてよく使われるフォーマットであるため。 
- 多くのプログラミング言語でサポートされているため、異なるシステム間でデータを共有する際に便利であるため。

## 使い方
JSONをPHPで扱う方法を具体的なコーディング例とともに紹介します。まずは、以下のようなJSONデータがあるとします。

```PHP
$data = '{ "name": "Yumi", "age": 28, "hobbies": ["hiking", "cooking", "reading"] }';
```

JSONデータをPHPで扱うには、まずは`json_decode()`関数を使ってデータを展開します。次のようにコードを書くことで、`$data`の内容を連想配列として取得できます。

```PHP
$json_array = json_decode($data, true);
```

`$json_array`の内容は以下のようになります。

```PHP
Array(
    [name] => Yumi
    [age] => 28
    [hobbies] => Array(
        [0] => hiking
        [1] => cooking
        [2] => reading
    )
)
```

さらに、連想配列から特定の値を取得するには、次のように書きます。

```PHP
echo $json_array['name']; // Yumi
echo $json_array['age']; // 28
```

また、配列の中に配列が入っている場合は、次のように書きます。

```PHP
echo $json_array['hobbies'][0]; // hiking
```

このように、`json_decode()`関数を使うことで簡単にJSONデータをPHPで扱うことができます。

## ディープダイブ
JSONはデータを階層的に表現することができるため、複雑なデータを効率的に扱うことができます。また、`json_encode()`関数を使うことで、PHPの配列をJSON形式に変換することもできます。

さらに、JSONにはさまざまなメソッド（`json_last_error()`や`json_last_error_msg()`など）があり、エラーが発生した場合でも追跡や対処ができるようになっています。

JSONについてさらに詳しく学びたい方は、公式ドキュメントを参考にしてみてください。

## 併せて見る
- [PHPの公式サイト：json_decode()関数](https://www.php.net/manual/ja/function.json-decode.php)
- [JSON入門 - CodeZine](https://codezine.jp/article/corner/558)