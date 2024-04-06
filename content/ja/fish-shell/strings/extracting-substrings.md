---
date: 2024-01-20 17:45:47.874348-07:00
description: "How to (\u65B9\u6CD5) Fish Shell\u3067\u306F\u3001`string` \u30B3\u30DE\
  \u30F3\u30C9\u304C\u30D0\u30FC\u30B8\u30E7\u30F3 2.3.0 \u304B\u3089\u5229\u7528\u53EF\
  \u80FD\u3067\u3059\u3002\u4ED6\u306E\u30B7\u30A7\u30EB\u8A00\u8A9E\u3068\u6BD4\u3079\
  \u3001Fish\u306F\u69CB\u6587\u304C\u7C21\u5358\u3067\u76F4\u611F\u7684\u3002\u4F3C\
  \u305F\u3088\u3046\u306A\u51E6\u7406\u304C `cut` \u3084 `awk`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:50:56.584929-06:00'
model: gpt-4-1106-preview
summary: "How to (\u65B9\u6CD5) Fish Shell\u3067\u306F\u3001`string` \u30B3\u30DE\u30F3\
  \u30C9\u304C\u30D0\u30FC\u30B8\u30E7\u30F3 2.3.0 \u304B\u3089\u5229\u7528\u53EF\u80FD\
  \u3067\u3059\u3002\u4ED6\u306E\u30B7\u30A7\u30EB\u8A00\u8A9E\u3068\u6BD4\u3079\u3001\
  Fish\u306F\u69CB\u6587\u304C\u7C21\u5358\u3067\u76F4\u611F\u7684\u3002\u4F3C\u305F\
  \u3088\u3046\u306A\u51E6\u7406\u304C `cut` \u3084 `awk` \u30B3\u30DE\u30F3\u30C9\
  \u3067\u53EF\u80FD\u3067\u3059\u304C\u3001`string`\u3092\u4F7F\u3046\u65B9\u304C\
  Fish\u3067\u306F\u81EA\u7136\u3067\u3059\u3002\u30B5\u30D6\u30B9\u30C8\u30EA\u30F3\
  \u30B0\u306E\u62BD\u51FA\u306F `-s`\uFF08\u958B\u59CB\u4F4D\u7F6E\u306E\u6307\u5B9A\
  \uFF09\u3068 `-l`\uFF08\u9577\u3055\u306E\u6307\u5B9A\uFF09\u30AA\u30D7\u30B7\u30E7\
  \u30F3\u3067\u7BA1\u7406\u3057\u307E\u3059\u3002\u3053\u306E\u76F4\u611F\u7684\u306A\
  \u30A2\u30D7\u30ED\u30FC\u30C1\u306F\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u304C\u7D20\
  \u65E9\u304F\u76EE\u7684\u306E\u6587\u5B57\u5217\u3092\u64CD\u4F5C\u3067\u304D\u308B\
  \u3088\u3046\u306B\u8A2D\u8A08\u3055\u308C\u3066\u3044\u307E\u3059\u3002"
title: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA"
weight: 6
---

## How to (方法)
```Fish Shell
# サブストリング抽出の例:
set my_string "Fish Shell is fun!"
# 6文字目から10文字目を取り出す
echo $my_string | string sub -s 6 -l 5
```
出力: `Shell`

```Fish Shell
# 文字列の先頭から4文字を取り出す
echo $my_string | string sub -l 4
```
出力: `Fish`

```Fish Shell
# 文字列の末尾から3文字を取り出す
echo $my_string | string sub -s -3
```
出力: `n!`

## Deep Dive (深掘り)
Fish Shellでは、`string` コマンドがバージョン 2.3.0 から利用可能です。他のシェル言語と比べ、Fishは構文が簡単で直感的。似たような処理が `cut` や `awk` コマンドで可能ですが、`string`を使う方がFishでは自然です。サブストリングの抽出は `-s`（開始位置の指定）と `-l`（長さの指定）オプションで管理します。この直感的なアプローチはプログラマーが素早く目的の文字列を操作できるように設計されています。

## See Also (関連情報)
- Fish公式ドキュメント: [https://fishshell.com/docs/current/](https://fishshell.com/docs/current/)
- `string`コマンドについての詳細: [https://fishshell.com/docs/current/cmds/string.html](https://fishshell.com/docs/current/cmds/string.html)
- シェルスクリプトチュートリアル: [https://www.learnshell.org/](https://www.learnshell.org/)
