---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:53.436767-07:00
description: "\u65B9\u6CD5\uFF1A \u305D\u306E\u6838\u3068\u306A\u308B\u90E8\u5206\u3067\
  \u3001Bash\u306F\u6761\u4EF6\u6587\u3068`-d`\u30AA\u30DA\u30EC\u30FC\u30BF\u30FC\
  \u3092\u4F7F\u7528\u3057\u3066\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u306E\u5B58\u5728\
  \u3092\u30C1\u30A7\u30C3\u30AF\u3059\u308B\u3053\u3068\u3092\u53EF\u80FD\u306B\u3057\
  \u307E\u3059\u3002\u4EE5\u4E0B\u306F\u3001\u3053\u306E\u30C1\u30A7\u30C3\u30AF\u3092\
  \u3069\u306E\u3088\u3046\u306B\u5B9F\u884C\u3059\u308B\u304B\u3092\u793A\u3059\u7C21\
  \u6F54\u306A\u4F8B\u3067\u3059\u3002"
lastmod: '2024-04-05T22:38:41.907747-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A \u305D\u306E\u6838\u3068\u306A\u308B\u90E8\u5206\u3067\
  \u3001Bash\u306F\u6761\u4EF6\u6587\u3068`-d`\u30AA\u30DA\u30EC\u30FC\u30BF\u30FC\
  \u3092\u4F7F\u7528\u3057\u3066\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u306E\u5B58\u5728\
  \u3092\u30C1\u30A7\u30C3\u30AF\u3059\u308B\u3053\u3068\u3092\u53EF\u80FD\u306B\u3057\
  \u307E\u3059\u3002\u4EE5\u4E0B\u306F\u3001\u3053\u306E\u30C1\u30A7\u30C3\u30AF\u3092\
  \u3069\u306E\u3088\u3046\u306B\u5B9F\u884C\u3059\u308B\u304B\u3092\u793A\u3059\u7C21\
  \u6F54\u306A\u4F8B\u3067\u3059\u3002"
title: "\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\u308B\u304B\u3069\
  \u3046\u304B\u306E\u78BA\u8A8D"
weight: 20
---

## 方法：
その核となる部分で、Bashは条件文と`-d`オペレーターを使用してディレクトリの存在をチェックすることを可能にします。以下は、このチェックをどのように実行するかを示す簡潔な例です。

```bash
if [ -d "/path/to/directory" ]; then
    echo "ディレクトリは存在します。"
else
    echo "ディレクトリは存在しません。"
fi
```

サンプル出力（ディレクトリが存在する場合）：
```
ディレクトリは存在します。
```

サンプル出力（ディレクトリが存在しない場合）：
```
ディレクトリは存在しません。
```

より複雑なスクリプトでは、存在しない場合にディレクトリを作成するなど、他の操作と組み合わせることが一般的です：

```bash
DIR="/path/to/directory"
if [ -d "$DIR" ]; then
    echo "$DIR は存在します。"
else
    echo "$DIR は存在しません。作成します..."
    mkdir -p "$DIR"
    echo "$DIR を作成しました。"
fi
```

サンプル出力（ディレクトリが存在しない場合、その後作成される）：
```
/path/to/directory は存在しません。作成します...
/path/to/directory を作成しました。
```

Bash自体がこのようなチェックのための強力なツールを提供しているため、ディレクトリの存在検証のために、特に人気のあるサードパーティ製のライブラリは存在しません。ネイティブのBashコマンドは、このタスクのために完全に有能で効率的です。
