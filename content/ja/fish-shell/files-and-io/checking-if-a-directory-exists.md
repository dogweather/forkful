---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:29.459544-07:00
description: "\u65B9\u6CD5\uFF1A Fish Shell\u306F`test`\u30B3\u30DE\u30F3\u30C9\u3092\
  \u4F7F\u3063\u3066\u30D5\u30A1\u30A4\u30EB\u30BF\u30A4\u30D7\u3084\u7279\u6027\u3092\
  \u30C1\u30A7\u30C3\u30AF\u3057\u307E\u3059\u3002\u3053\u308C\u306B\u306F\u3001\u5BFE\
  \u8C61\u304C\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304B\u3069\u3046\u304B\u3092\u542B\
  \u307F\u307E\u3059\u3002\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\
  \u308B\u304B\u3092\u78BA\u8A8D\u3059\u308B\u57FA\u672C\u7684\u306A\u65B9\u6CD5\u306F\
  \u4EE5\u4E0B\u306E\u3068\u304A\u308A\u3067\u3059\uFF1A."
lastmod: '2024-03-13T22:44:42.756765-06:00'
model: gpt-4-0125-preview
summary: "Fish Shell\u306F`test`\u30B3\u30DE\u30F3\u30C9\u3092\u4F7F\u3063\u3066\u30D5\
  \u30A1\u30A4\u30EB\u30BF\u30A4\u30D7\u3084\u7279\u6027\u3092\u30C1\u30A7\u30C3\u30AF\
  \u3057\u307E\u3059\u3002\u3053\u308C\u306B\u306F\u3001\u5BFE\u8C61\u304C\u30C7\u30A3\
  \u30EC\u30AF\u30C8\u30EA\u304B\u3069\u3046\u304B\u3092\u542B\u307F\u307E\u3059\u3002\
  \u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\u308B\u304B\u3092\u78BA\
  \u8A8D\u3059\u308B\u57FA\u672C\u7684\u306A\u65B9\u6CD5\u306F\u4EE5\u4E0B\u306E\u3068\
  \u304A\u308A\u3067\u3059\uFF1A."
title: "\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\u308B\u304B\u3069\
  \u3046\u304B\u306E\u78BA\u8A8D"
weight: 20
---

## 方法：
Fish Shellは`test`コマンドを使ってファイルタイプや特性をチェックします。これには、対象がディレクトリかどうかを含みます。ディレクトリが存在するかを確認する基本的な方法は以下のとおりです：

```fish
if test -d /path/to/dir
    echo "ディレクトリは存在します"
else
    echo "ディレクトリは存在しません"
end
```
サンプル出力：
```
ディレクトリは存在します
```

より合理化されたファイルやディレクトリ操作には、`fd`のような外部ツールを使うこともありますが、これは存在を単にチェックするよりも、ファイルやディレクトリを見つけるためによく使われます。しかし、Fishスクリプティングと組み合わせることで便利な結果が得られます：

```fish
set dir "/path/to/search"
if fd . $dir --type directory --max-depth 1 | grep -q $dir
    echo "ディレクトリは存在します"
else
    echo "ディレクトリは存在しません"
end
```

この`fd`の例では、指定した深さでディレクトリを検索し、`grep`が一致をチェックします。これは洗練されたチェックに対して多用途です。しかし、存在を直接確認する目的には、Fishのビルトイン`test`を使うことが、効率的で明快です。
