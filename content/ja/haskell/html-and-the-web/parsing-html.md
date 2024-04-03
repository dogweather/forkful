---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:25.878398-07:00
description: "\u65B9\u6CD5: Haskell\u3067HTML\u3092\u89E3\u6790\u3059\u308B\u306B\u306F\
  \u3001\u305D\u306E\u5358\u7D14\u3055\u3068\u67D4\u8EDF\u6027\u304B\u3089`tagsoup`\u30E9\
  \u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002\u307E\u305A\u3001\
  \u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u306Ecabal\u30D5\u30A1\u30A4\u30EB\u306B`tagsoup`\u3092\
  \u8FFD\u52A0\u3059\u308B\u304B\u3001`cabal install tagsoup`\u3092\u5B9F\u884C\u3057\
  \u3066\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\u3057\
  \u3066\u304F\u3060\u3055\u3044\u3002"
lastmod: '2024-03-13T22:44:42.180525-06:00'
model: gpt-4-0125-preview
summary: "Haskell\u3067HTML\u3092\u89E3\u6790\u3059\u308B\u306B\u306F\u3001\u305D\u306E\
  \u5358\u7D14\u3055\u3068\u67D4\u8EDF\u6027\u304B\u3089`tagsoup`\u30E9\u30A4\u30D6\
  \u30E9\u30EA\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002\u307E\u305A\u3001\u30D7\u30ED\
  \u30B8\u30A7\u30AF\u30C8\u306Ecabal\u30D5\u30A1\u30A4\u30EB\u306B`tagsoup`\u3092\
  \u8FFD\u52A0\u3059\u308B\u304B\u3001`cabal install tagsoup`\u3092\u5B9F\u884C\u3057\
  \u3066\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\u3057\
  \u3066\u304F\u3060\u3055\u3044."
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
