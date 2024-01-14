---
title:                "Fish Shell: 文字列のキャピタライズ"
simple_title:         "文字列のキャピタライズ"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列を大文字に変換する必要性について、丁寧な説明はないでしょう。プログラミングにおいて文字列を大文字に変換することはしばしば重要な処理であり、多くの場合、ユーザーが見やすいフォーマットを提供するために必要です。そのため、Fish Shellで文字列を大文字に変換する方法を学ぶことは、より効率的なコーディングを行うために重要です。

## 方法

まず、大文字に変換したい文字列を定義します。例として、"hello world"という文字列を使用しましょう。次に、Fish Shellのコマンドプロンプトに以下のコードを入力します。

```
set my_string "hello world"
echo $my_string | tr '[:lower:]' '[:upper:]'
```

上記のコードでは、まず変数`my_string`を定義し、その後`tr`コマンドを使用して大文字に変換します。これにより、出力として"HELLO WORLD"が表示されます。実際のコーディングでは、このように変数を使用せずに直接文字列を引数として渡すこともできます。

## ディープダイブ

Fish Shellの`tr`コマンドは、文字列を変換するために利用される重要なコマンドです。`tr`コマンドは、指定された文字を置き換える機能を持つため、大文字化以外にもさまざまな用途に使用することができます。また、`[:lower:]`や`[:upper:]`といった正規表現を利用することで、特定の文字列のみを変換することも可能です。

## 参考リンク

- [Fish Shell公式サイト](https://fishshell.com/)
- [Fish Shellドキュメンテーション](https://fishshell.com/docs/current/index.html)
- [シンガポール大学によるFish Shellチュートリアル動画（英語）](https://www.youtube.com/watch?v=KhnbxIkfofo) 

## 関連リンク

- [正規表現についての記事（日本語）](https://qiita.com/kidach1/items/7635d82434cc6e4ad0a5)
- [UNIXコマンドについてのサイト（日本語）](https://www.infraexpert.com/study/unix3.html)