---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:41.306129-07:00
description: "YAML\u3092\u64CD\u4F5C\u3059\u308B\u3068\u306F\u3001\u8A2D\u5B9A\u30D5\
  \u30A1\u30A4\u30EB\u7528\u306E\u30C7\u30FC\u30BF\u76F4\u5217\u5316\u30D5\u30A9\u30FC\
  \u30DE\u30C3\u30C8\u3067\u3042\u308BYAML\uFF08YAML Ain't Markup Language\uFF09\u30D5\
  \u30A1\u30A4\u30EB\u3092\u30D1\u30FC\u30B7\u30F3\u30B0\u3057\u3001Fish\u2026"
lastmod: '2024-03-13T22:44:42.765233-06:00'
model: gpt-4-0125-preview
summary: "YAML\u3092\u64CD\u4F5C\u3059\u308B\u3068\u306F\u3001\u8A2D\u5B9A\u30D5\u30A1\
  \u30A4\u30EB\u7528\u306E\u30C7\u30FC\u30BF\u76F4\u5217\u5316\u30D5\u30A9\u30FC\u30DE\
  \u30C3\u30C8\u3067\u3042\u308BYAML\uFF08YAML Ain't Markup Language\uFF09\u30D5\u30A1\
  \u30A4\u30EB\u3092\u30D1\u30FC\u30B7\u30F3\u30B0\u3057\u3001Fish Shell\u5185\u3067\
  \u64CD\u4F5C\u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\u308C\u3092\u884C\u3046\u3053\u3068\u3067\
  \u3001\u30B7\u30A7\u30EB\u74B0\u5883\u306E\u6587\u8108\u5185\u3067\u30A2\u30D7\u30EA\
  \u30B1\u30FC\u30B7\u30E7\u30F3\u3084\u30B5\u30FC\u30D3\u30B9\u3092\u52B9\u7387\u3088\
  \u304F\u81EA\u52D5\u5316\u3001\u8A2D\u5B9A\u3059\u308B\u3053\u3068\u304C\u3067\u304D\
  \u3001\u69CB\u6210\u7BA1\u7406\u3084\u30EA\u30BD\u30FC\u30B9\u306E\u30D7\u30ED\u30D3\
  \u30B8\u30E7\u30CB\u30F3\u30B0\u3068\u3044\u3063\u305F\u30BF\u30B9\u30AF\u3092\u5BB9\
  \u6613\u306B\u3057\u307E\u3059\u3002."
title: "YAML \u3092\u64CD\u4F5C\u3059\u308B"
weight: 41
---

## 何となぜ？
YAMLを操作するとは、設定ファイル用のデータ直列化フォーマットであるYAML（YAML Ain't Markup Language）ファイルをパーシングし、Fish Shell内で操作することを意味します。プログラマーはこれを行うことで、シェル環境の文脈内でアプリケーションやサービスを効率よく自動化、設定することができ、構成管理やリソースのプロビジョニングといったタスクを容易にします。

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
