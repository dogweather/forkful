---
title:                "「JSONを扱う」"
html_title:           "Bash: 「JSONを扱う」"
simple_title:         "「JSONを扱う」"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/working-with-json.md"
---

{{< edit_this_page >}}

# JSONを使う理由と方法

## What & Why?
JSONとは、データの構造化フォーマットの一種です。プログラマーたちがJSONを使用する理由は、データを簡単かつ柔軟に扱うことができるからです。

## How to:
JSONを使用するためには、Bashのコマンドラインで"jq"パッケージをインストールする必要があります。次のコマンドを実行すると、インストールが始まります。

```
sudo apt install jq
```

JSONデータを解析するには、Bashのシェルスクリプトで"jq"コマンドを使用します。以下は、シンプルな例です。

```
echo '{"name": "John", "age": 30}' | jq '.'
```

出力：
```
{
     "name": "John",
     "age": 30
}
```

## Deep Dive
JSONは1999年にダグラス・クロックフォードにより開発されました。JSONは、XMLよりも簡単な文法を持ち、WebアプリケーションやAPI、データベースとのインターフェースで広く使用されています。

JSONの代替としては、XMLやYAMLなどがありますが、JSONが最も一般的に使用されています。

JSONを使用する際、Bashのシェルスクリプトでは"jq"以外にもパッケージやライブラリを使用することができます。例えば、"gron"というパッケージを使用することで、JSONをプレーンテキストに変換することができます。

## See Also
- Official "jq" Documentation: https://stedolan.github.io/jq/manual/
- "gron" GitHub Repository: https://github.com/tomnomnom/gron