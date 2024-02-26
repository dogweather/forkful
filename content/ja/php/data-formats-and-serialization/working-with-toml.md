---
date: 2024-01-26 04:24:44.193873-07:00
description: "TOML\u306F\u3001Tom's Obvious, Minimal Language\u306E\u7565\u3067\u3001\
  JSON\u3084YAML\u306B\u4F3C\u305F\u30C7\u30FC\u30BF\u5F62\u5F0F\u3067\u3059\u304C\
  \u3001\u4EBA\u9593\u304C\u8AAD\u3080\u306B\u306F\u3088\u308A\u7C21\u5358\u3067\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u305D\u308C\u304C\u76F4\u622A\
  \u3067\u30C7\u30FC\u30BF\u69CB\u9020\u306B\u3046\u307E\u304F\u5909\u63DB\u3055\u308C\
  \u308B\u305F\u3081\u3001\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u306B\u4F7F\u7528\u3057\
  \u307E\u3059\u3002"
lastmod: '2024-02-25T18:49:40.272388-07:00'
model: gpt-4-0125-preview
summary: "TOML\u306F\u3001Tom's Obvious, Minimal Language\u306E\u7565\u3067\u3001\
  JSON\u3084YAML\u306B\u4F3C\u305F\u30C7\u30FC\u30BF\u5F62\u5F0F\u3067\u3059\u304C\
  \u3001\u4EBA\u9593\u304C\u8AAD\u3080\u306B\u306F\u3088\u308A\u7C21\u5358\u3067\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u305D\u308C\u304C\u76F4\u622A\
  \u3067\u30C7\u30FC\u30BF\u69CB\u9020\u306B\u3046\u307E\u304F\u5909\u63DB\u3055\u308C\
  \u308B\u305F\u3081\u3001\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u306B\u4F7F\u7528\u3057\
  \u307E\u3059\u3002"
title: "TOML\u3092\u6271\u3046\u65B9\u6CD5"
---

{{< edit_this_page >}}

## 何となぜ？
TOMLは、Tom's Obvious, Minimal Languageの略で、JSONやYAMLに似たデータ形式ですが、人間が読むにはより簡単です。プログラマーは、それが直截でデータ構造にうまく変換されるため、設定ファイルに使用します。

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
