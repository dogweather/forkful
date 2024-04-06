---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:53.705967-07:00
description: "\u4F7F\u3044\u65B9: \u4F8B\u306B\u5165\u308B\u524D\u306B\u3001`yq`\u304C\
  \u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\u3055\u308C\u3066\u3044\u308B\u3053\u3068\u3092\
  \u78BA\u8A8D\u3057\u3066\u304F\u3060\u3055\u3044\u3002\u305F\u3044\u3066\u3044\u306E\
  \u5834\u5408\u3001\u30D1\u30C3\u30B1\u30FC\u30B8\u30DE\u30CD\u30FC\u30B8\u30E3\u30FC\
  \u304B\u3089\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\u3067\u304D\u307E\u3059\u3002\u4F8B\
  \u3048\u3070\u3001Ubuntu\u306E\u5834\u5408\uFF1A."
lastmod: '2024-04-05T21:53:43.231508-06:00'
model: gpt-4-0125-preview
summary: "Bash\u3067\u76F4\u63A5YAML\u3092\u6271\u3046\u306B\u306F\u3001Bash\u306B\
  \u306FYAML\u306E\u89E3\u6790\u3092\u30B5\u30DD\u30FC\u30C8\u3059\u308B\u6A5F\u80FD\
  \u304C\u7D44\u307F\u8FBC\u307E\u308C\u3066\u3044\u306A\u3044\u305F\u3081\u3001\u3061\
  \u3087\u3063\u3068\u3057\u305F\u5DE5\u592B\u304C\u5FC5\u8981\u3067\u3059\u3002\u3057\
  \u304B\u3057\u3001`yq`\u306E\u3088\u3046\u306A\u5916\u90E8\u30C4\u30FC\u30EB\uFF08\
  \u8EFD\u91CF\u3067\u30DD\u30FC\u30BF\u30D6\u30EB\u306A\u30B3\u30DE\u30F3\u30C9\u30E9\
  \u30A4\u30F3YAML\u30D7\u30ED\u30BB\u30C3\u30B5\uFF09\u3092\u4F7F\u7528\u3057\u3066\
  \u3001\u52B9\u7387\u7684\u306BYAML\u30D5\u30A1\u30A4\u30EB\u3068\u3084\u308A\u53D6\
  \u308A\u3059\u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u3044\u304F\u3064\
  \u304B\u306E\u4E00\u822C\u7684\u306A\u64CD\u4F5C\u3092\u898B\u3066\u307F\u307E\u3057\
  \u3087\u3046\uFF1A."
title: "YAML \u3092\u64CD\u4F5C\u3059\u308B"
weight: 41
---

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
