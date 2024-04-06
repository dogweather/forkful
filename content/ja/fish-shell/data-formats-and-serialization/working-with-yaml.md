---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:41.306129-07:00
description: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A Fish Shell\u306B\u306F\
  YAML\u3092\u30D1\u30FC\u30B7\u30F3\u30B0\u3059\u308B\u305F\u3081\u306E\u7D44\u307F\
  \u8FBC\u307F\u30B5\u30DD\u30FC\u30C8\u306F\u3042\u308A\u307E\u305B\u3093\u304C\u3001\
  `yq`\uFF08\u8EFD\u91CF\u3067\u30DD\u30FC\u30BF\u30D6\u30EB\u306A\u30B3\u30DE\u30F3\
  \u30C9\u30E9\u30A4\u30F3YAML\u30D7\u30ED\u30BB\u30C3\u30B5\uFF09\u3068\u3044\u3063\
  \u305F\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u30C4\u30FC\u30EB\u3092\u5229\u7528\
  \u3059\u308B\u3053\u3068\u3067YAML\u30C7\u30FC\u30BF\u3092\u6271\u3046\u3053\u3068\
  \u304C\u3067\u304D\u307E\u3059\u3002\u2026"
lastmod: '2024-04-05T22:38:42.247504-06:00'
model: gpt-4-0125-preview
summary: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A Fish Shell\u306B\u306F\
  YAML\u3092\u30D1\u30FC\u30B7\u30F3\u30B0\u3059\u308B\u305F\u3081\u306E\u7D44\u307F\
  \u8FBC\u307F\u30B5\u30DD\u30FC\u30C8\u306F\u3042\u308A\u307E\u305B\u3093\u304C\u3001\
  `yq`\uFF08\u8EFD\u91CF\u3067\u30DD\u30FC\u30BF\u30D6\u30EB\u306A\u30B3\u30DE\u30F3\
  \u30C9\u30E9\u30A4\u30F3YAML\u30D7\u30ED\u30BB\u30C3\u30B5\uFF09\u3068\u3044\u3063\
  \u305F\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u30C4\u30FC\u30EB\u3092\u5229\u7528\
  \u3059\u308B\u3053\u3068\u3067YAML\u30C7\u30FC\u30BF\u3092\u6271\u3046\u3053\u3068\
  \u304C\u3067\u304D\u307E\u3059\u3002 **yq\u306E\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\
  \uFF08\u307E\u3060\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\u3055\u308C\u3066\u3044\u306A\
  \u3044\u5834\u5408\uFF09\uFF1A**."
title: "YAML \u3092\u64CD\u4F5C\u3059\u308B"
weight: 41
---

## どのようにして：
Fish ShellにはYAMLをパーシングするための組み込みサポートはありませんが、`yq`（軽量でポータブルなコマンドラインYAMLプロセッサ）といったサードパーティツールを利用することでYAMLデータを扱うことができます。

**yqのインストール（まだインストールされていない場合）：**
```fish
sudo apt-get install yq
```

**YAMLファイルから値を読み取る：**
次の内容を持つYAMLファイル`config.yaml`があるとします：
```yaml
database:
  host: localhost
  port: 3306
```

データベースホストを読み取るには、次を使用します：
```fish
set host (yq e '.database.host' config.yaml)
echo $host
```
**サンプル出力：**
```
localhost
```

**YAMLファイル内の値を更新する：**
`port`を`5432`に更新するには、次を使用します：
```fish
yq e '.database.port = 5432' -i config.yaml
```
**更新を確認する：**
```fish
yq e '.database.port' config.yaml
```
**サンプル出力：**
```
5432
```

**新しいYAMLファイルを書き込む：**
事前定義された内容で新しい`new_config.yaml`を作成するには：
```fish
echo "webserver:
  host: '127.0.0.1'
  port: 8080" | yq e -P - > new_config.yaml
```
この操作は`yq`を使用して、文字列を新しいYAMLファイルに処理し、きれいに印刷（-Pフラグ）します。

**複雑な構造をパーシングする：**
より複雑なYAMLファイルを扱い、ネストされた配列やオブジェクトを取得する必要がある場合、次を実行できます：
```fish
echo "servers:
  - name: server1
    ip: 192.168.1.101
  - name: server2
    ip: 192.168.1.102" > servers.yaml

yq e '.servers[].name' servers.yaml
```
**サンプル出力：**
```
server1
server2
```
`yq`を使用して、Fish ShellはYAMLドキュメントを通して効率的にナビゲートし、さまざまな自動化や設定タスクのためにそれらを操作することを簡単にします。
