---
title:                "文字列から引用符を削除する"
aliases:
- ja/fish-shell/removing-quotes-from-a-string.md
date:                  2024-01-26T03:39:03.401502-07:00
model:                 gpt-4-0125-preview
simple_title:         "文字列から引用符を削除する"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列から引用符を削除するとは、テキストデータからそのうるさい単一（' '）または二重（" "）引用符を取り除くことです。プログラマーは通常、入力を正規化したり、引用符のごちゃごちゃを避けてさらなる処理のためにデータを準備するためにこれを行います。

## 方法:

Fishには、この種のタスク用の内蔵マジックがあります。`string`関数を使って、汗をかくことなく使用してください。これらの呪文をチェックしてください：

```fish
# 単一引用符の例
set quoted "'Hello, World!'"
set unquoted (string trim --chars \"\'\" $quoted)
echo $unquoted # 出力：Hello, World!

# 二重引用符でも同じ扱い
set double_quoted "\"Hello, Universe!\""
set unquoted (string trim --chars \"\'\" $double_quoted)
echo $unquoted # 出力：Hello, Universe!
```

## 深く掘り下げて

コマンドラインの石器時代には、`sed`や`awk`を使って引用符を削除するために苦労しました；バックスラッシュや謎のフラグの本当のからみ合いです。Fishの`string`関数は、より新しい時代から来ており、コードをよりクリーンで直感的にします。

他のシェルの代替方法では、これらの古いツールに依然として依存しているか、bashのパラメーター展開やzshの修正子のような独自の組み込みメソッドを使用するかもしれません。

`string`関数は、引用符をトリミングすることを超えています。文字列操作のためのFishにおけるスイスアーミーナイフです。`string`を使うと、端末内で文字列をスライス、ダイス、スプリット、結合、またはさえも正規表現マッチングができます。

## 参照

公式ドキュメントを参考に`string`をさらに深く掘り下げてください：
- [Fish Shell String ドキュメント](https://fishshell.com/docs/current/commands.html#string)

懐かしさのため、またはより伝統的なシェルでスクリプトを作成する場合には、次をチェックしてください：
- [Sed & Awk ガイド](https://www.grymoire.com/Unix/Sed.html)
- [Bash パラメータ展開](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
