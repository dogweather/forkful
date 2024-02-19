---
aliases:
- /ja/fish-shell/working-with-toml/
date: 2024-01-26 04:21:46.345282-07:00
description: "TOML\u306F\u3001\u4EBA\u9593\u304C\u8AAD\u307F\u66F8\u304D\u3057\u3084\
  \u3059\u304F\u3001\u30DE\u30B7\u30F3\u304C\u89E3\u6790\u304A\u3088\u3073\u751F\u6210\
  \u3057\u3084\u3059\u3044\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u5F62\u5F0F\u3067\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u8AAD\u307F\u3084\u3059\u3055\
  \u304C\u91CD\u8981\u306A\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u3067\u3001\u660E\u78BA\
  \u3067\u968E\u5C64\u7684\u306A\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u3092\u4F5C\u6210\
  \u3059\u308B\u305F\u3081\u306BTOML\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002"
lastmod: 2024-02-18 23:08:55.331092
model: gpt-4-0125-preview
summary: "TOML\u306F\u3001\u4EBA\u9593\u304C\u8AAD\u307F\u66F8\u304D\u3057\u3084\u3059\
  \u304F\u3001\u30DE\u30B7\u30F3\u304C\u89E3\u6790\u304A\u3088\u3073\u751F\u6210\u3057\
  \u3084\u3059\u3044\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u5F62\u5F0F\u3067\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u8AAD\u307F\u3084\u3059\u3055\u304C\
  \u91CD\u8981\u306A\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u3067\u3001\u660E\u78BA\u3067\
  \u968E\u5C64\u7684\u306A\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u3092\u4F5C\u6210\u3059\
  \u308B\u305F\u3081\u306BTOML\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002"
title: "TOML\u3092\u6271\u3046\u65B9\u6CD5"
---

{{< edit_this_page >}}

## 何となぜか？
TOMLは、人間が読み書きしやすく、マシンが解析および生成しやすい設定ファイル形式です。プログラマーは、読みやすさが重要なプロジェクトで、明確で階層的な設定ファイルを作成するためにTOMLを使用します。

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
