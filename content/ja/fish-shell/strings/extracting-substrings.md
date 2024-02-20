---
date: 2024-01-20 17:45:47.874348-07:00
description: "\u6587\u5B57\u5217\u304B\u3089\u7279\u5B9A\u90E8\u5206\u3092\u53D6\u308A\
  \u51FA\u3059\u306E\u304C\u30B5\u30D6\u30B9\u30C8\u30EA\u30F3\u30B0\u62BD\u51FA\u3067\
  \u3059\u3002\u30C7\u30FC\u30BF\u306E\u89E3\u6790\u3001\u51E6\u7406\u3001\u7279\u5B9A\
  \u30D1\u30BF\u30FC\u30F3\u306E\u691C\u7D22\u306A\u3069\u306B\u5FC5\u8981\u3067\u3059\
  \u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:01.825544
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u304B\u3089\u7279\u5B9A\u90E8\u5206\u3092\u53D6\u308A\
  \u51FA\u3059\u306E\u304C\u30B5\u30D6\u30B9\u30C8\u30EA\u30F3\u30B0\u62BD\u51FA\u3067\
  \u3059\u3002\u30C7\u30FC\u30BF\u306E\u89E3\u6790\u3001\u51E6\u7406\u3001\u7279\u5B9A\
  \u30D1\u30BF\u30FC\u30F3\u306E\u691C\u7D22\u306A\u3069\u306B\u5FC5\u8981\u3067\u3059\
  \u3002"
title: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列から特定部分を取り出すのがサブストリング抽出です。データの解析、処理、特定パターンの検索などに必要です。

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
