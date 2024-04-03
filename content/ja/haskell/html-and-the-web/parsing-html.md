---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:25.878398-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.180525-06:00'
model: gpt-4-0125-preview
summary: "Haskell\u3067HTML\u3092\u89E3\u6790\u3059\u308B\u3053\u3068\u3067\u3001\u30C7\
  \u30FC\u30BF\u3092\u62BD\u51FA\u3057\u305F\u308A\u3001HTML\u30B3\u30F3\u30C6\u30F3\
  \u30C4\u3092\u64CD\u4F5C\u3057\u305F\u308A\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u3067\
  Web\u30DA\u30FC\u30B8\u3068\u5BFE\u8A71\u3057\u305F\u308A\u3059\u308B\u3053\u3068\
  \u304C\u3067\u304D\u307E\u3059\u3002\u3053\u306E\u64CD\u4F5C\u306F\u3001Web\u30B9\
  \u30AF\u30EC\u30A4\u30D4\u30F3\u30B0\u3001Web\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\
  \u30E7\u30F3\u306E\u81EA\u52D5\u30C6\u30B9\u30C8\u3001\u30A6\u30A7\u30D6\u30B5\u30A4\
  \u30C8\u304B\u3089\u306E\u30C7\u30FC\u30BF\u30DE\u30A4\u30CB\u30F3\u30B0\u306A\u3069\
  \u306E\u30BF\u30B9\u30AF\u306B\u3068\u3063\u3066\u4E0D\u53EF\u6B20\u3067\u3059\u3002\
  \u3053\u308C\u306B\u3088\u308A\u3001Haskell\u306E\u5F37\u529B\u306A\u578B\u30B7\u30B9\
  \u30C6\u30E0\u3068\u95A2\u6570\u578B\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u30D1\
  \u30E9\u30C0\u30A4\u30E0\u3092\u6D3B\u7528\u3057\u3066\u3001\u5805\u7262\u3067\u7C21\
  \u6F54\u306A\u30B3\u30FC\u30C9\u3092\u5B9F\u73FE\u3057\u307E\u3059\u3002."
title: "HTML\u306E\u89E3\u6790"
weight: 43
---

## 方法:
HaskellでHTMLを解析するには、その単純さと柔軟性から`tagsoup`ライブラリを使用します。まず、プロジェクトのcabalファイルに`tagsoup`を追加するか、`cabal install tagsoup`を実行してライブラリをインストールしてください。

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.TagSoup

-- デモンストレーション用のサンプルHTML
let sampleHtml = "<html><body><p>Learn Haskell!</p><a href='http://example.com'>Click Here</a></body></html>"

-- HTMLを解析しリンク（aタグ）をフィルターする
let tags = parseTags sampleHtml
let links = [fromAttrib "href" tag | tag <- tags, isTagOpenName "a" tag]

-- 抽出したリンクを印刷
print links
```

サンプル出力：
```plaintext
["http://example.com"]
```

より高度なHTML解析が必要な場合は、文書変換を扱っている場合などに`pandoc`ライブラリの使用を検討してください。これは非常に汎用性が高いですが、より複雑です：

```haskell
import Text.Pandoc

-- Pandocドキュメント（doc）を読み込んだと仮定して、例えば、ファイルから読み取ります
let doc = ... -- ここにPandocドキュメントが入ります

-- ドキュメントをHTML文字列に変換
let htmlString = writeHtmlString def doc

-- ここで、上述のように`htmlString`を解析するか、必要に応じて進めます。
```
`pandoc`は、多数のマークアップ形式間の変換に焦点を当てたはるかに大きなライブラリなので、それらの追加機能が必要であるか、またはアプリケーションで既に文書形式を扱っている場合に使用してください。
