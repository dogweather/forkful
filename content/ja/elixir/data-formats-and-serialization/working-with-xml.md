---
aliases:
- /ja/elixir/working-with-xml/
date: 2024-01-26 04:29:57.806471-07:00
description: "Elixir\u3067\u306EXML\u306E\u53D6\u308A\u6271\u3044\u3068\u306F\u3001\
  XML\u30C7\u30FC\u30BF\u306E\u89E3\u6790\u3001\u4F5C\u6210\u3001\u64CD\u4F5C\u306E\
  \u3053\u3068\u3092\u6307\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u3001Web\u30B5\u30FC\u30D3\u30B9\u3001\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\
  \u3001\u65E7\u30B7\u30B9\u30C6\u30E0\u306B\u5E83\u304F\u4F7F\u7528\u3055\u308C\u3066\
  \u3044\u308B\u305F\u3081\u3001XML\u3092\u6271\u3044\u307E\u3059\u3002"
lastmod: 2024-02-18 23:08:54.665718
model: gpt-4-0125-preview
summary: "Elixir\u3067\u306EXML\u306E\u53D6\u308A\u6271\u3044\u3068\u306F\u3001XML\u30C7\
  \u30FC\u30BF\u306E\u89E3\u6790\u3001\u4F5C\u6210\u3001\u64CD\u4F5C\u306E\u3053\u3068\
  \u3092\u6307\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\
  Web\u30B5\u30FC\u30D3\u30B9\u3001\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u3001\u65E7\
  \u30B7\u30B9\u30C6\u30E0\u306B\u5E83\u304F\u4F7F\u7528\u3055\u308C\u3066\u3044\u308B\
  \u305F\u3081\u3001XML\u3092\u6271\u3044\u307E\u3059\u3002"
title: "XML\u306E\u6271\u3044\u65B9"
---

{{< edit_this_page >}}

## 何となぜ？
ElixirでのXMLの取り扱いとは、XMLデータの解析、作成、操作のことを指します。プログラマーは、Webサービス、設定ファイル、旧システムに広く使用されているため、XMLを扱います。

## 方法:
Elixirには、標準ライブラリにXML解析機能は含まれていません。SweetXMLは人気のある選択肢です。使い方は以下の通りです：

```elixir
# mix.exsの依存関係にSweetXMLを追加
{:sweet_xml, "~> 0.6"}

# コード内で
import SweetXml

xml = """
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>Reminder</heading>
  <body>Don't forget me this weekend!</body>
</note>
"""

# XMLの解析
note = xml |> xpath(~x"//note")
to = xml |> xpath(~x"//note/to" |> inner_text())
IO.puts to # 出力: Tove
```

## 深掘り
XML、または拡張可能マークアップ言語は90年代後半から存在します。冗長ですが構造化されており、複雑なデータ交換に理想的です。JSONの簡潔さによる人気が急上昇する一方で、XMLはその表現力と標準化されたスキーマのために、多くの企業や金融システムで根強く存在しています。

代替案には以下があります：
- より軽量で冗長でないデータ交換のためのJSON。
- 特に内部システム用のバイナリシリアライズデータ通信のためのProtobufやThrift。

内部的には、ElixirのXMLライブラリは解析のためにErlangの:xmerlライブラリを利用しており、これは堅牢なサポートを提供しますが、より現代的なアプローチに比べ直感的ではないかもしれません。Elixirが進化するにつれて、SweetXMLのようなコミュニティ主導のライブラリはこれらをよりElixirらしい構文でラップし、XMLの操作をよりアクセスしやすくしています。

## 参照：
- SweetXML on Hex: https://hex.pm/packages/sweet_xml
- ElixirのXML解析に関する考え方: https://elixir-lang.org/getting-started/mix-otp/dependencies-and-umbrella-projects.html
- 基本的なXML処理についてのxmerlドキュメント: http://erlang.org/doc/apps/xmerl/index.html
