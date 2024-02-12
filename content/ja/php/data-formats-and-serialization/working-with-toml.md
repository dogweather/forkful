---
title:                "TOMLを扱う方法"
aliases:
- /ja/php/working-with-toml.md
date:                  2024-01-26T04:24:44.193873-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOMLを扱う方法"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/working-with-toml.md"
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
