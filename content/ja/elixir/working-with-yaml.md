---
title:                "yamlを使ったプログラミング"
html_title:           "Elixir: yamlを使ったプログラミング"
simple_title:         "yamlを使ったプログラミング"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

# YAMLを使ってみよう：丁寧なElixirプログラミング

## 何をするもので、なぜ使うのか？

YAMLとは、プログラムでデータを表現するための形式です。プログラマーたちがYAMLを使う理由は、コードをより読みやすく、管理しやすくするためです。また、YAMLは人間が手で編集できるので、設定ファイルやデータの保存にも便利です。

## 方法：コード例と出力

```Elixir
# Mapデータ型をYAMLに変換する
YAML.dump(%{name: "John", age: 35}, indent: 2)
```

出力：
```yaml
name: John
age: 35
```

## 手掘り：YAMLとの深い関わり方

- **歴史的背景**：YAMLは2001年に登場し、プログラムでデータを表現するための新しい方法として注目されました。
- **代替手段**：JSONやXMLなどの他のデータ形式もありますが、YAMLはその簡潔で人間が理解しやすい表現によって、注目を集めています。
- **実装の詳細**：Elixirでは、YAMLを扱うための便利なライブラリである「yamerl」が利用できます。このライブラリはNIF（Native Implemented Functions）を使用しており、高速にYAMLデータを処理することができます。

## さらに見る：関連情報へのリンク

- yamerlライブラリのGitHubリポジトリ：https://github.com/yakaz/yamerl
- Elixirの公式ドキュメント：https://hexdocs.pm/elixir/YAML.html