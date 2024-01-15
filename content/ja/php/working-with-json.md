---
title:                "jsonを使用する"
html_title:           "PHP: jsonを使用する"
simple_title:         "jsonを使用する"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/working-with-json.md"
---

{{< edit_this_page >}}

## なぜ 

JSONを使ってプログラミングするのは、データの受け渡しや保存によく使われる形式で、PHPのような言語では簡単に扱うことができるからです。

## 使い方 

JSONをPHPで扱うには、`json_encode()`と`json_decode()`という関数を使います。例えば、次のコードでは、PHPの配列をJSON形式で出力し、そのJSONを再度PHPの配列に変換しています。 

```PHP 
$input = ["apple", "orange", "banana"];
$json = json_encode($input); 
$output = json_decode($json);
var_dump($output); 
```

出力は次のようになります。 

```PHP 
array(3) {
  [0]=> string(5) "apple"
  [1]=> string(6) "orange"
  [2]=> string(6) "banana"
}
```

## 深堀り 

JSONを扱う際に注意する点として、データのエスケープやフォーマットについてです。特にユーザーの入力を含むデータをJSONに変換する場合は、`json_encode()`に`JSON_HEX_TAG`や`JSON_HEX_QUOT`などのオプションを指定して、HTMLタグやクォーテーションをエスケープするようにすると安全です。また、`json_decode()`では第二引数に`true`を指定することで、返り値を連想配列に変換することができます。

## 参考リンク 

- [PHP: JSON 関数 - Manual](https://www.php.net/manual/ja/ref.json.php)
- [JSON - Wikipedia](https://ja.wikipedia.org/wiki/JSON)

## 参考になるリンク 

- [安全なJSONエンコーディング・デコーディング]https://developer.okta.com/blog/2019/02/04/create-and-understand-json-web-tokens)
- [PHPとJSONの相互変換方法]https://www.tutorialspoint.com/php/php_json.htm)
- [PHPにおけるソート済みJSON出力方法]https://stackoverflow.com/questions/43847233/php-output-multidimensional-array-as-sorted-json)