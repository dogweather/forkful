---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:07.289705-07:00
description: "Elixir\u3067\u6A19\u6E96\u30A8\u30E9\u30FC(stderr)\u3078\u306E\u66F8\
  \u304D\u8FBC\u307F\u306F\u3001\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\u30FC\u30B8\u3084\
  \u8A3A\u65AD\u3092\u30E1\u30A4\u30F3\u51FA\u529B(stdout)\u3068\u306F\u5225\u306B\
  \u6307\u793A\u3059\u308B\u65B9\u6CD5\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306Fstderr\u3092\u4F7F\u7528\u3057\u3066\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\
  \u306E\u30E1\u30A4\u30F3\u51FA\u529B\u3092 clutter\u3059\u308B\u3053\u3068\u306A\
  \u304F\u30A8\u30E9\u30FC\u3092\u30C7\u30D0\u30C3\u30B0\u3057\u3001\u51E6\u7406\u3059\
  \u308B\u305F\u3081\u3001\u554F\u984C\u3092\u7279\u5B9A\u3057\u5BFE\u51E6\u3057\u3084\
  \u3059\u304F\u306A\u308A\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.633281-06:00'
model: gpt-4-0125-preview
summary: "Elixir\u3067\u6A19\u6E96\u30A8\u30E9\u30FC(stderr)\u3078\u306E\u66F8\u304D\
  \u8FBC\u307F\u306F\u3001\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\u30FC\u30B8\u3084\u8A3A\
  \u65AD\u3092\u30E1\u30A4\u30F3\u51FA\u529B(stdout)\u3068\u306F\u5225\u306B\u6307\
  \u793A\u3059\u308B\u65B9\u6CD5\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306Fstderr\u3092\u4F7F\u7528\u3057\u3066\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u306E\
  \u30E1\u30A4\u30F3\u51FA\u529B\u3092 clutter\u3059\u308B\u3053\u3068\u306A\u304F\
  \u30A8\u30E9\u30FC\u3092\u30C7\u30D0\u30C3\u30B0\u3057\u3001\u51E6\u7406\u3059\u308B\
  \u305F\u3081\u3001\u554F\u984C\u3092\u7279\u5B9A\u3057\u5BFE\u51E6\u3057\u3084\u3059\
  \u304F\u306A\u308A\u307E\u3059\u3002"
title: "\u6A19\u6E96\u30A8\u30E9\u30FC\u3078\u306E\u66F8\u304D\u8FBC\u307F"
---

{{< edit_this_page >}}

## 何となぜ？

Elixirで標準エラー(stderr)への書き込みは、エラーメッセージや診断をメイン出力(stdout)とは別に指示する方法です。プログラマーはstderrを使用して、プログラムのメイン出力を clutterすることなくエラーをデバッグし、処理するため、問題を特定し対処しやすくなります。

## 方法:

Elixirでは、`IO.puts/2` や `IO.warn/2` などの`IO`モジュールの関数を使用して、標準エラーにメッセージを書き込むことができます：

```elixir
# stderrにシンプルなメッセージを書き込む
IO.puts(:stderr, "Error: 何かがうまくいかなかった！")

# 警告/エラーに対してより意味的なIO.warnを使用
IO.warn("警告：限界を超えようとしています！")
```

`IO.puts/2`のターミナルでのサンプル出力：
```
Error: 何かがうまくいかなかった！
```

`IO.warn/2`の場合、出力は似ていますが、`IO.warn/2`は特に警告用に設計されており、将来のElixirバージョンでは追加のフォーマッティングや振る舞いが含まれるかもしれません。

**サードパーティのライブラリの使用**

標準エラー出力の処理にはElixirの標準ライブラリが通常十分ですが、より複雑なアプリケーションや異なるログレベルや出力を設定するために、`Logger`のようなライブラリが便利であることがあります。

エラーメッセージを出力するために`Logger`を使用した例：

```elixir
require Logger

# Loggerをstderrに出力するように設定
Logger.configure_backend(:console, device: :stderr)

# エラーメッセージを書き込む
Logger.error("Error: データベースに接続できなかった。")
```

この設定は、`Logger`の出力を特にstderrに指示するもので、エラーロギングを標準のログメッセージから分離するのに役立ちます。
