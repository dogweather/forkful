---
title:                "ウェブページのダウンロード"
aliases:
- ja/vba/downloading-a-web-page.md
date:                  2024-02-01T21:52:58.404584-07:00
model:                 gpt-4-0125-preview
simple_title:         "ウェブページのダウンロード"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/vba/downloading-a-web-page.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

Visual Basic for Applications（VBA）でウェブページをダウンロードすることは、インターネットからウェブページのHTML内容を取得することを含みます。プログラマーはしばしば、Excel、Access、または他のOfficeアプリケーション内から、プログラム的にウェブサイトの内容を処理または分析するためにこの作業を行います。

## 方法

VBAでウェブページをダウンロードするには、サーバーHTTPリクエストを有効にするMicrosoft XML、v6.0（MSXML6）ライブラリを利用できます。コードに入る前に、VBAエディタで `ツール` -> `参照設定` に進んで、`Microsoft XML, v6.0`がチェックされていることを確認しましょう。

ウェブページのHTML内容をダウンロードする簡単な例をこちらに示します：

```basic
Sub DownloadWebPage()
    Dim request As Object
    Dim url As String
    Dim response As String
    
    ' XML HTTPリクエストオブジェクトを初期化する
    Set request = CreateObject("MSXML2.XMLHTTP")
    
    url = "http://www.example.com"
    
    ' 同期リクエストを開く
    request.Open "GET", url, False
    
    ' サーバーへリクエストを送信する
    request.send
    
    ' レスポンステキストを取得する
    response = request.responseText
    
    ' レスポンスを即時ウィンドウに出力する（デバッグ目的用）
    Debug.Print response
    
    ' クリーンアップ
    Set request = Nothing
End Sub
```

このサブルーチンを実行すると、VBAエディタの即時ウィンドウに`http://www.example.com`のHTMLが出力されます。`Open`メソッドの`False`パラメータはリクエストを同期的にすることを意味しており、次の行へ移動する前にウェブページがダウンロードされるまでコードが待機することを意味します。

## 深掘り

示されている技術は、MSXMLに依存しています。これは、MicrosoftのXML HTTP Request標準の実装であり、ウェブ開発でAJAXリクエストにしばしば使用されます。このコンポーネントは長い間Microsoftの技術スタックの一部であり、VBAでのネットワークリクエストにとって堅牢な選択肢です。

しかし、VBAとMSXMLへの依存は、JavaScriptを大量に使用し動的なコンテンツのレンダリングを行う現代のウェブアプリケーションで制限が発生する可能性があります。これらの制限は、JavaScriptを実行し複雑なウェブサイトの相互作用を扱う能力を持つ、Pythonのような他の言語やBeautifulSoupやSeleniumなどのライブラリがウェブスクレイピングタスクにより適している理由です。

それにもかかわらず、単純なタスクで直接HTML内容を取得する場合や、Officeアプリケーションの制約内で作業する場合には、VBAは実用的なツールのままです。Officeスイート内での統合は、ウェブコンテンツに基づいてドキュメントを直接操作するための独自の利点を提供し、特定のユースケースには便利です。
