---
date: 2024-01-26 03:39:03.401502-07:00
description: "\u65B9\u6CD5: Fish\u306B\u306F\u3001\u3053\u306E\u7A2E\u306E\u30BF\u30B9\
  \u30AF\u7528\u306E\u5185\u8535\u30DE\u30B8\u30C3\u30AF\u304C\u3042\u308A\u307E\u3059\
  \u3002`string`\u95A2\u6570\u3092\u4F7F\u3063\u3066\u3001\u6C57\u3092\u304B\u304F\
  \u3053\u3068\u306A\u304F\u4F7F\u7528\u3057\u3066\u304F\u3060\u3055\u3044\u3002\u3053\
  \u308C\u3089\u306E\u546A\u6587\u3092\u30C1\u30A7\u30C3\u30AF\u3057\u3066\u304F\u3060\
  \u3055\u3044\uFF1A."
lastmod: '2024-03-13T22:44:42.714835-06:00'
model: gpt-4-0125-preview
summary: "Fish\u306B\u306F\u3001\u3053\u306E\u7A2E\u306E\u30BF\u30B9\u30AF\u7528\u306E\
  \u5185\u8535\u30DE\u30B8\u30C3\u30AF\u304C\u3042\u308A\u307E\u3059\u3002`string`\u95A2\
  \u6570\u3092\u4F7F\u3063\u3066\u3001\u6C57\u3092\u304B\u304F\u3053\u3068\u306A\u304F\
  \u4F7F\u7528\u3057\u3066\u304F\u3060\u3055\u3044\u3002\u3053\u308C\u3089\u306E\u546A\
  \u6587\u3092\u30C1\u30A7\u30C3\u30AF\u3057\u3066\u304F\u3060\u3055\u3044\uFF1A."
title: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u524A\u9664\u3059\u308B"
weight: 9
---

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
