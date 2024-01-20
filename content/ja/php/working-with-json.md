---
title:                "JSONを扱う方法"
html_title:           "Arduino: JSONを扱う方法"
simple_title:         "JSONを扱う方法"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

JSONはデータ交換のフォーマット。読みやすく、機械が解析しやすい。PHPプログラマーはAPI連携や設定ファイルなどでJSONを使う。

## How to: (方法)

### JSONをエンコードする：
```PHP
<?php
$data = ['name' => 'Taro', 'age' => 25];
$json = json_encode($data);
echo $json;
?>
```

出力：
```JSON
{"name":"Taro","age":25}
```

### JSONをデコードする：
```PHP
<?php
$json = '{"name":"Taro","age":25}';
$data = json_decode($json);
print_r($data);
?>
```

出力：
```
stdClass Object
(
    [name] => Taro
    [age] => 25
)
```

### JSONデコード結果を連想配列にする：
```PHP
<?php
$json = '{"name":"Taro","age":25}';
$data = json_decode($json, true);
print_r($data);
?>
```

出力：
```
Array
(
    [name] => Taro
    [age] => 25
)
```

## Deep Dive (掘り下げ)

JSON（JavaScript Object Notation）は2001年に登場。軽量であるため、XMLの代わりとして使われることが多い。PHPでは`json_encode()`と`json_decode()`を使い、エンコードやデコードを行う。PHP 5.2.0から標準でJSON機能が追加された。連想配列としてデコードするかオブジェクトとしてデコードするかは、`json_decode()`の第二引数で制御する。

## See Also (関連情報)

- PHPの公式ドキュメントのJSON関数: [PHP: JSON Functions](https://www.php.net/manual/en/ref.json.php)
- JSONについてのより詳細な情報: [JSON.org](https://www.json.org/json-en.html)