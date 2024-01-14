---
title:    "Fish Shell: ディレクトリが存在するかを確認する"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## なぜ

ディレクトリが存在するかどうかを確認することの重要性について説明します。

## ハウツー

```Fish Shell```を使用したコーディングの例と、実際の出力を以下に示します。

### 例1：ディレクトリが存在する場合

```
if test -d /path/to/directory
  echo "ディレクトリが存在します。"
end
```

#### 出力

```
ディレクトリが存在します。
```

### 例2：ディレクトリが存在しない場合

```
if test -d /path/to/nonexistent/directory
  echo "ディレクトリは存在しません。"
end
```

#### 出力

```
ディレクトリは存在しません。
```

## ディープダイブ

ディレクトリが存在するかどうかを確認するコマンドは、```test -d```です。これは、指定されたパスがディレクトリである場合にのみ真を返します。このコマンドは、スクリプト内で条件分岐を行うために使用されることが多いです。

また、```set -x```を使用して実行されるコマンドを表示することもできます。これにより、コマンドがどのように実行されているかを確認することができます。

## 参考リンク

- [Fish Shell documentaion on test command](https://fishshell.com/docs/current/cmds/test.html)
- [An article on conditional statements in Fish Shell](https://www.digitalocean.com/community/tutorials/how-to-use-conditionals-in-fish-shell-scripting)
- [A helpful video tutorial on basic Fish Shell commands](https://www.youtube.com/watch?v=pSKs57QA9ck)

## この記事について

この記事では、```Fish Shell```を使用してディレクトリが存在するかどうかを確認する方法について学びました。これは、スクリプト内で条件分岐を行う際に非常に便利です。さらに詳細を知りたい場合は、参考リンクの記事やチュートリアルを参考にしてください。

## 関連記事

- [Fish Shellの使い方入門](https://techacademy.jp/magazine/57547)
- [Fish Shellでの変数と環境変数の設定方法](https://qiita.com/masashi127/items/641ff5d65e217224a325)
- [Fish Shellの便利なプラグインまとめ](https://repsy.info/archives/1088)