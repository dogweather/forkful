---
title:                "ディレクトリが存在するかどうかの確認"
aliases:
- ja/fish-shell/checking-if-a-directory-exists.md
date:                  2024-02-03T19:07:29.459544-07:00
model:                 gpt-4-0125-preview
simple_title:         "ディレクトリが存在するかどうかの確認"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
Fish Shellでディレクトリが存在するかを確認することは、ディレクトリ構造の存在や不在に基づいてスクリプトが判断を行うことを可能にし、条件付きファイル操作、ログ記録、または環境設定のようなタスクを可能にします。この技術は、ファイルシステムと予測可能な方法でやり取りする堅牢なスクリプトを書くためには欠かせません。

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
