---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:58.755117-07:00
description: "\u4F7F\u3044\u65B9\uFF1A PHP\u3067\u306EJSON\u306E\u53D6\u308A\u6271\
  \u3044\u306F\u3001\u7D44\u307F\u8FBC\u307F\u95A2\u6570`json_encode()`\u3068`json_decode()`\u306E\
  \u304A\u304B\u3052\u3067\u7C21\u5358\u3067\u3059\u3002\u4EE5\u4E0B\u306B\u3001PHP\u914D\
  \u5217\u3092JSON\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B\u4F8B\u3068\u305D\
  \u306E\u9006\u306E\u4F8B\u3092\u793A\u3057\u307E\u3059\uFF1A."
lastmod: '2024-04-05T22:38:41.804074-06:00'
model: gpt-4-0125-preview
summary: "\u4F7F\u3044\u65B9\uFF1A PHP\u3067\u306EJSON\u306E\u53D6\u308A\u6271\u3044\
  \u306F\u3001\u7D44\u307F\u8FBC\u307F\u95A2\u6570`json_encode()`\u3068`json_decode()`\u306E\
  \u304A\u304B\u3052\u3067\u7C21\u5358\u3067\u3059\u3002\u4EE5\u4E0B\u306B\u3001PHP\u914D\
  \u5217\u3092JSON\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B\u4F8B\u3068\u305D\
  \u306E\u9006\u306E\u4F8B\u3092\u793A\u3057\u307E\u3059\uFF1A."
title: "JSON\u3092\u6D3B\u7528\u3059\u308B"
weight: 38
---

## 使い方：
PHPでのJSONの取り扱いは、組み込み関数`json_encode()`と`json_decode()`のおかげで簡単です。以下に、PHP配列をJSON文字列に変換する例とその逆の例を示します：

### PHP配列をJSON文字列にエンコードする
```php
// 連想配列を定義
$data = [
    "name" => "John Doe",
    "age" => 30,
    "email" => "john.doe@example.com"
];

// PHP配列をJSON文字列に変換
$jsonString = json_encode($data);

// JSON文字列を出力
echo $jsonString;
```
**サンプル出力：**
```json
{"name":"John Doe","age":30,"email":"john.doe@example.com"}
```

### JSON文字列をPHP配列にデコードする
```php
// JSON文字列
$jsonString = '{"name":"John Doe","age":30,"email":"john.doe@example.com"}';

// JSON文字列をPHP配列に変換
$data = json_decode($jsonString, true);

// PHP配列を出力
print_r($data);
```
**サンプル出力：**
```
Array
(
    [name] => John Doe
    [age] => 30
    [email] => john.doe@example.com
)
```

### サードパーティライブラリであるGuzzleHttpを利用する
複雑なJSONおよびウェブリクエストの処理には、人気のPHPライブラリであるGuzzleHttpが使用されます。このライブラリはHTTPリクエストを簡素化し、JSONデータを簡単に扱うことができます。

**Composerを使用したインストール：**
```
composer require guzzlehttp/guzzle
```

**リクエスト例：**
```php
require 'vendor/autoload.php';

use GuzzleHttp\Client;

$client = new Client();

// JSONを返すAPIへのリクエストを送信
$response = $client->request('GET', 'https://api.example.com/data', [
    'headers' => [
        'Accept' => 'application/json',
    ],
]);

// JSONレスポンスをPHP配列にデコード
$data = json_decode($response->getBody(), true);

// データを出力
print_r($data);
```

**APIが類似のJSONデータを返すことを前提として：**
```
Array
(
    [name] => John Doe
    [age] => 30
    [email] => john.doe@example.com
)
```
これは、ネイティブ関数とGuzzleHttpのような堅牢なライブラリを使用して、より複雑なタスクに対処する場合も含め、PHPでJSON操作を容易に行うことを示しています。
