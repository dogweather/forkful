---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:56.494157-07:00
description: "\u6A19\u6E96\u30A8\u30E9\u30FC\uFF08stderr\uFF09\u3078\u306E\u66F8\u304D\
  \u8FBC\u307F\u3068\u306F\u3001\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\u30FC\u30B8\u3084\
  \u8A3A\u65AD\u51FA\u529B\u3092\u6A19\u6E96\u51FA\u529B\uFF08stdout\uFF09\u3068\u306F\
  \u5225\u306E\u30C1\u30E3\u30F3\u30CD\u30EB\u306B\u5411\u3051\u308B\u3053\u3068\u3067\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\u308C\u3092\u884C\u3046\
  \u3053\u3068\u3067\u3001\u901A\u5E38\u306E\u30D7\u30ED\u30B0\u30E9\u30E0\u7D50\u679C\
  \u3068\u30A8\u30E9\u30FC\u60C5\u5831\u3092\u533A\u5225\u3057\u3001\u30C7\u30D0\u30C3\
  \u30B0\u3068\u30ED\u30B0\u8A18\u9332\u306E\u30D7\u30ED\u30BB\u30B9\u3092\u5408\u7406\
  \u5316\u3057\u307E\u3059\u3002"
lastmod: 2024-02-19 22:05:01.461259
model: gpt-4-0125-preview
summary: "\u6A19\u6E96\u30A8\u30E9\u30FC\uFF08stderr\uFF09\u3078\u306E\u66F8\u304D\
  \u8FBC\u307F\u3068\u306F\u3001\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\u30FC\u30B8\u3084\
  \u8A3A\u65AD\u51FA\u529B\u3092\u6A19\u6E96\u51FA\u529B\uFF08stdout\uFF09\u3068\u306F\
  \u5225\u306E\u30C1\u30E3\u30F3\u30CD\u30EB\u306B\u5411\u3051\u308B\u3053\u3068\u3067\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\u308C\u3092\u884C\u3046\
  \u3053\u3068\u3067\u3001\u901A\u5E38\u306E\u30D7\u30ED\u30B0\u30E9\u30E0\u7D50\u679C\
  \u3068\u30A8\u30E9\u30FC\u60C5\u5831\u3092\u533A\u5225\u3057\u3001\u30C7\u30D0\u30C3\
  \u30B0\u3068\u30ED\u30B0\u8A18\u9332\u306E\u30D7\u30ED\u30BB\u30B9\u3092\u5408\u7406\
  \u5316\u3057\u307E\u3059\u3002"
title: "\u6A19\u6E96\u30A8\u30E9\u30FC\u3078\u306E\u66F8\u304D\u8FBC\u307F"
---

{{< edit_this_page >}}

## 何となぜ？
標準エラー（stderr）への書き込みとは、エラーメッセージや診断出力を標準出力（stdout）とは別のチャンネルに向けることです。プログラマーはこれを行うことで、通常のプログラム結果とエラー情報を区別し、デバッグとログ記録のプロセスを合理化します。

## 方法：
Luaでstderrに書き込むには、`io.stderr:write()`関数を使用します。こちらが標準エラーに単純なエラーメッセージを書き込む方法です：

```lua
io.stderr:write("Error: Invalid input.\n")
```

変数を出力する必要がある場合や、複数のデータを組み合わせる場合は、write関数内でそれらを連結します：

```lua
local errorMessage = "Invalid input."
io.stderr:write("Error: " .. errorMessage .. "\n")
```

**stderr上のサンプル出力：**
```
Error: Invalid input.
```

さらに複雑なシナリオや、より大規模なアプリケーションで作業している場合は、LuaLoggingのようなサードパーティのログ記録ライブラリを検討するかもしれません。LuaLoggingを使用すると、stderrを含む異なる先にログを向けることができます。こちらが簡単な例です：

まず、LuaRocksを使用してLuaLoggingがインストールされていることを確認します：

```
luarocks install lualogging
```

次に、LuaLoggingを使用してstderrにエラーメッセージを書き込むには：

```lua
local logging = require("logging")
local logger = logging.stderr()
logger:error("Error: Invalid input.")
```

このアプローチは、アプリケーション全体で標準化されたログ記録の利点を提供し、簡単なAPIを介してログレベル（例：ERROR、WARN、INFO）を設定する柔軟性を追加します。
