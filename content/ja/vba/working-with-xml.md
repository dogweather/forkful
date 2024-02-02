---
title:                "XMLとの作業"
date:                  2024-02-01T22:07:15.479344-07:00
model:                 gpt-4-0125-preview
simple_title:         "XMLとの作業"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/vba/working-with-xml.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ?

Visual Basic for Applications (VBA) で XML を扱うことは、Microsoft Office アプリケーションの文脈内で XML ドキュメントを解析、作成、および変更することを含みます。プログラマーは、Office アプリケーションを XML を出力する Web サービスや他のデータソースと統合し、データ交換やレポーティング機能を容易にするために、この機能を利用します。

## 方法:

XML を扱い始めるには、通常 `MSXML2.DOMDocument` オブジェクトを使用します。このインターフェースを使用すると、XML ドキュメントをロード、解析、およびナビゲートすることができます。以下は、XML ファイルをロードし、その構造をナビゲートし、属性とテキスト内容を読む方法を示す簡単な例です。

```basic
' まず、ツール -> 参照を通して "Microsoft XML, v6.0" への参照を追加したことを確認してください
Dim xmlDoc As MSXML2.DOMDocument60
Set xmlDoc = New MSXML2.DOMDocument60
xmlDoc.async = False
xmlDoc.Load("C:\Path\To\Your\File.xml") ' あなたの XML ファイルをロード

' XML が正常にロードされたかを確認
If xmlDoc.parseError.ErrorCode <> 0 Then
    MsgBox "XML ロードエラー:" & xmlDoc.parseError.reason
Else
    '  要素をナビゲートして読む
    Dim book As IXMLDOMNode
    Set book = xmlDoc.SelectSingleNode("//book/title") ' <book>  内の最初の <title> を見つけるための XPath
    MsgBox book.Text ' タイトルテキストを表示
End If
```

上記のサンプルコードでは、`MSXML2.DOMDocument60` のインスタンスを作成し、XML ファイルをロードしてから、エラーをチェックします。エラーが見つからない場合は、XPath を使用して特定のノードにナビゲートし、そのテキスト内容を表示します。

## 深堀り:

VBA における XML 機能の統合は、Office アプリケーションが Web データおよびサービスとやり取りする必要性が高まった2000 年代初頭にさかのぼります。`MSXML` ライブラリ、または Microsoft XML Core Services は年々進化し、改善されたパフォーマンスとセキュリティ機能を備える最新バージョンとして `MSXML2.DOMDocument60` が推奨されています。

しかし、VBA の XML 処理機能は強力であるにもかかわらず、Python の XML.etree や C#'s LINQ to XML など、現代のプログラミング環境に比べて効率が低く、より煩雑であると考えられています。VBA の固有の冗長性と参照を手動で追加および管理する必要性が、迅速な開発を妨げる可能性があります。さらに、JSON がより軽量なデータ交換フォーマットとして登場して以来、伝統的なシステムや特定のエンタープライズサービスとの互換性を必要とする場合を除き、多くのプログラマーやアプリケーションが XML から離れています。

しかしながら、Microsoft Office の自動化の文脈で XML ドキュメントの解析や生成が必要なタスクにおいて、VBA の XML 処理機能を活用することは、実行可能であり、時には必要なアプローチです。これにより、Office アプリケーションの豊富な機能セットと XML によって提供される構造化されたデータ操作機能へのアクセスのバランスが取れます。
