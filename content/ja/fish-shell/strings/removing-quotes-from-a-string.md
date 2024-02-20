---
date: 2024-01-26 03:39:03.401502-07:00
description: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u524A\u9664\u3059\
  \u308B\u3068\u306F\u3001\u30C6\u30AD\u30B9\u30C8\u30C7\u30FC\u30BF\u304B\u3089\u305D\
  \u306E\u3046\u308B\u3055\u3044\u5358\u4E00\uFF08' '\uFF09\u307E\u305F\u306F\u4E8C\
  \u91CD\uFF08\" \"\uFF09\u5F15\u7528\u7B26\u3092\u53D6\u308A\u9664\u304F\u3053\u3068\
  \u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u901A\u5E38\u3001\u5165\
  \u529B\u3092\u6B63\u898F\u5316\u3057\u305F\u308A\u3001\u5F15\u7528\u7B26\u306E\u3054\
  \u3061\u3083\u3054\u3061\u3083\u3092\u907F\u3051\u3066\u3055\u3089\u306A\u308B\u51E6\
  \u7406\u306E\u305F\u3081\u306B\u30C7\u30FC\u30BF\u3092\u6E96\u5099\u3059\u308B\u305F\
  \u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
lastmod: 2024-02-19 22:05:01.824313
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u524A\u9664\u3059\
  \u308B\u3068\u306F\u3001\u30C6\u30AD\u30B9\u30C8\u30C7\u30FC\u30BF\u304B\u3089\u305D\
  \u306E\u3046\u308B\u3055\u3044\u5358\u4E00\uFF08' '\uFF09\u307E\u305F\u306F\u4E8C\
  \u91CD\uFF08\" \"\uFF09\u5F15\u7528\u7B26\u3092\u53D6\u308A\u9664\u304F\u3053\u3068\
  \u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u901A\u5E38\u3001\u5165\
  \u529B\u3092\u6B63\u898F\u5316\u3057\u305F\u308A\u3001\u5F15\u7528\u7B26\u306E\u3054\
  \u3061\u3083\u3054\u3061\u3083\u3092\u907F\u3051\u3066\u3055\u3089\u306A\u308B\u51E6\
  \u7406\u306E\u305F\u3081\u306B\u30C7\u30FC\u30BF\u3092\u6E96\u5099\u3059\u308B\u305F\
  \u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u524A\u9664\u3059\u308B"
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
