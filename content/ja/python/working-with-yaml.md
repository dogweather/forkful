---
title:                "Python: YAMLを使ったプログラミング"
simple_title:         "YAMLを使ったプログラミング"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## なぜ

YAMLという形式を使用してプログラミングを行う理由は、設定やデータを簡潔なテキストファイルに保存することができるためです。そしてそのテキストファイルをコードから読み取ることができます。

## ハウツー

YAMLを使って設定ファイルを作成するには、Pythonのライブラリであるpyyamlをインストールする必要があります。次に、設定ファイルを読み込むためのコードを書くことができます。

```Python
# pyyamlをインポートする
import yaml 

# 設定ファイルを開く
with open('config.yml') as f:
  # 読み込んだファイルをyaml形式としてパースする
  config = yaml.load(f)

# パースしたデータを使用する
print(config['database']['username'])
```

この例では、config.ymlという名前のファイルからデータを読み込み、そのデータを表示する方法を示しています。

## ディープダイブ

YAMLは、プログラムの設定だけでなく、データのストレージやコードの構造化にも使用されることがあります。YAMLはさまざまなデータ型をサポートしており、インデントを使用して階層的な構造を作ることができます。さらに、YAMLはコメントをサポートしており、設定ファイルをより親切にすることができます。

## 参考リンク

- pyyamlのドキュメント：https://pyyaml.org/wiki/PyYAMLDocumentation
- 設定ファイルの例：https://github.com/mikekelly/halcyon/blob/master/package.yml
- YAMLのチュートリアル：https://rollout.io/blog/yaml-tutorial-everything-you-need-get-started/

## その他

この記事では、YAMLを使ったプログラミングの方法について説明しました。YAMLは非常に便利な形式であり、より簡単な方法でデータと設定を扱うことができます。ぜひ今後のプログラミングに活用してみてください。