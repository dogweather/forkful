---
date: 2024-01-26 04:21:46.345282-07:00
description: "\u65B9\u6CD5 Fish\u3067TOML\u3092\u8AAD\u307F\u53D6\u308A\u3001\u64CD\
  \u4F5C\u3059\u308B\u305F\u3081\u306B\u3001`yj`\u306E\u3088\u3046\u306A\u30C4\u30FC\
  \u30EB\u3092\u4F7F\u7528\u3059\u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\
  \u3053\u308C\u306FTOML\u3092JSON\u306B\u5909\u63DB\u3067\u304D\u307E\u3059\u3002\
  \u4EE5\u4E0B\u304C\u305D\u306E\u65B9\u6CD5\u3067\u3059\uFF1A."
lastmod: '2024-04-05T21:53:43.549654-06:00'
model: gpt-4-0125-preview
summary: ''
title: "TOML\u3092\u6271\u3046\u65B9\u6CD5"
weight: 39
---

## 方法
FishでTOMLを読み取り、操作するために、`yj`のようなツールを使用することができます。これはTOMLをJSONに変換できます。以下がその方法です：

```fish
# Fisher経由でyjをインストール
fisher install jorgebucaran/yj

# TOMLをJSONに変換
echo 'title = "TOML Example"' | yj -tj

# サンプル出力
{"title":"TOML Example"}
```

TOMLを書くには、プロセスを逆にします：

```fish
# JSONをTOMLに変換
echo '{"title":"JSON Example"}' | yj -jt

# サンプル出力
title = "JSON Example"
```

大規模な作業では、`toml-cli`のような専用のTOML CLIツールを検討してください。

```fish
# toml-cliをインストール
pip install toml-cli

# TOMLファイルに値を設定
toml set pyproject.toml tool.poetry.version "1.1.4"

# TOMLファイルから値を取得
set version (toml get pyproject.toml tool.poetry.version)
echo $version
```

## 深堀り
TOML（Tom's Obvious, Minimal Language）は、2013年にTom Preston-Wernerによって導入され、INIに似ていますが、定義された仕様とデータ階層があります。主要な代替手段はJSONとYAMLですが、トレードオフがあります：JSONは人間にとってあまり友好的ではない一方、YAMLはより複雑です。TOMLの設計は、設定ファイルが手作業で頻繁に保守されるシナリオで優れており、シンプルさと表現力のバランスをとります。実装に関しては、ほとんどのプログラミング言語でTOMLパーサーが利用可能であり、スクリプトに直接組み込むことができるFish用のTomlBombadilなどがあります。

## 参照
- TOML公式仕様: https://toml.io
- `yj`, TOML、JSON、YAML、およびXML間で変換するツール: https://github.com/jorgebucaran/yj
- `toml-cli`, TOML用のコマンドラインユーティリティ: https://github.com/sdispater/toml-cli
