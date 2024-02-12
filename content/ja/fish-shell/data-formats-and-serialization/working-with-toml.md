---
title:                "TOMLを扱う方法"
aliases:
- /ja/fish-shell/working-with-toml/
date:                  2024-01-26T04:21:46.345282-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOMLを扱う方法"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/working-with-toml.md"
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
