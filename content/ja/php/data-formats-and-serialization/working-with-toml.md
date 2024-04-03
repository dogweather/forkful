---
date: 2024-01-26 04:24:44.193873-07:00
description: "\u65B9\u6CD5\uFF1A \u307E\u305A\u3001`yosymfony/toml`\u306E\u3088\u3046\
  \u306ATOML\u30D1\u30FC\u30B5\u30FC\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u30A4\u30F3\
  \u30B9\u30C8\u30FC\u30EB\u3057\u3066\u3044\u308B\u3053\u3068\u3092\u78BA\u8A8D\u3057\
  \u307E\u3059\u3002TOML\u30D5\u30A1\u30A4\u30EB\u3092\u89E3\u6790\u3057\u307E\u3057\
  \u3087\u3046\uFF1A."
lastmod: '2024-03-13T22:44:42.277408-06:00'
model: gpt-4-0125-preview
summary: "\u307E\u305A\u3001`yosymfony/toml`\u306E\u3088\u3046\u306ATOML\u30D1\u30FC\
  \u30B5\u30FC\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\
  \u3057\u3066\u3044\u308B\u3053\u3068\u3092\u78BA\u8A8D\u3057\u307E\u3059\u3002TOML\u30D5\
  \u30A1\u30A4\u30EB\u3092\u89E3\u6790\u3057\u307E\u3057\u3087\u3046\uFF1A."
title: "TOML\u3092\u6271\u3046\u65B9\u6CD5"
weight: 39
---

## 方法：
まず、`yosymfony/toml`のようなTOMLパーサーライブラリをインストールしていることを確認します。TOMLファイルを解析しましょう：

```php
composer require yosymfony/toml

<?php
require 'vendor/autoload.php';

use Yosymfony\Toml\Toml;

$tomlString = <<<TOML
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
TOML;

$array = Toml::Parse($tomlString);

print_r($array);
```

サンプル出力：

```
Array
(
    [database] => Array
        (
            [server] => 192.168.1.1
            [ports] => Array
                (
                    [0] => 8001
                    [1] => 8001
                    [2] => 8002
                )

            [connection_max] => 5000
            [enabled] => 1
        )

)
```

## 深く掘り下げる
TOMLは2013年に、GitHubの共同創設者であるTom Preston-Wernerによって、設定ファイル用のXMLやJSONよりもユーザーフレンドリーな代替手段として作成されました。JSONがマシンにとって簡単であるのに対し、TOMLの構造は人間の目に優しく、YAMLの複雑さを持っていません。

TOMLの代わりとなるものには、JSON、YAML、XMLがあります。それぞれには強みと適用シナリオがあります。JSONは普遍的で言語に依存しないです；YAMLはより読みやすくコメントをサポートしています；一方、XMLは広範囲にサポートされています。

PHPでTOMLを実装する場合、内容をPHPの配列やオブジェクトに解析するライブラリを見ています。`yosymfony/toml`はTOMLスペックのv0.4.0に準拠するPHPパーサーです。最新のものに追いつくために、常に最新のTOMLバージョン(v1.0.0が私の最後の更新時)をサポートする新しいパーサーや更新をチェックしてください。

## 参照
- TOML仕様：<https://toml.io/>
- PHPのためのTOMLパーサー (`yosymfony/toml`)：<https://github.com/yosymfony/toml>
- データ形式の比較 (XML、JSON、YAML、TOML)：<https://www.loginradius.com/blog/engineering/comparing-data-interchange-formats/>
- PHPパッケージマネージャー (Composer)：<https://getcomposer.org/>
