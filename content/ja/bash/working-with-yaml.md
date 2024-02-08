---
title:                "YAML を操作する"
aliases:
- ja/bash/working-with-yaml.md
date:                  2024-02-03T19:24:53.705967-07:00
model:                 gpt-4-0125-preview
simple_title:         "YAML を操作する"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

YAMLは、「YAML Ain't Markup Language」を意味し、人間が読み書き可能なデータシリアライゼーション標準で、設定ファイルやデータの保存や転送が行われるアプリケーションで使用することができます。プログラマーは、YAMLの明快さと単純さ、特に複雑な設定や簡単に編集可能なデータ構造が必要なプロジェクトにおいて、YAMLを好んで使用します。

## 使い方:

Bashで直接YAMLを扱うには、BashにはYAMLの解析をサポートする機能が組み込まれていないため、ちょっとした工夫が必要です。しかし、`yq`のような外部ツール（軽量でポータブルなコマンドラインYAMLプロセッサ）を使用して、効率的にYAMLファイルとやり取りすることができます。いくつかの一般的な操作を見てみましょう：

### `yq`のインストール:

例に入る前に、`yq`がインストールされていることを確認してください。たいていの場合、パッケージマネージャーからインストールできます。例えば、Ubuntuの場合：

```bash
sudo apt-get install yq
```

または、GitHubリポジトリから直接ダウンロードすることもできます。

### 値の読み取り:

`config.yaml`という名前のファイルがあり、以下の内容が含まれているとします：

```yaml
database:
  host: localhost
  port: 5432
user:
  name: admin
  password: secret
```

データベースのホストを読み取るには、次のように`yq`を使用します：

```bash
yq e '.database.host' config.yaml
```

**サンプル出力:**

```
localhost
```

### 値の更新:

`config.yaml`のユーザー名を更新するには、`-i`（インプレース）オプションを付けて`yq eval`コマンドを使用します：

```bash
yq e '.user.name = "newadmin"' -i config.yaml
```

次で変更を確認します：

```bash
yq e '.user.name' config.yaml
```

**サンプル出力:**

```
newadmin
```

### 新しい要素の追加:

データベースセクションに、新たなフィールド`timeout`を追加するには：

```bash
yq e '.database.timeout = 30' -i config.yaml
```

ファイルの内容を確認すると、追加されていることが確認できます。

### 要素の削除:

ユーザーのパスワードを削除するには：

```bash
yq e 'del(.user.password)' -i config.yaml
```

この操作により、設定からパスワードフィールドが削除されます。

`yq`は強力なツールであり、YAMLをJSONに変換する、ファイルをマージする、さらに複雑な操作を行うなど、さらに多くの機能を持っています。さらに探求するためには、`yq`のドキュメントを参照してください。
