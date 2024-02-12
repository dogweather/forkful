---
title:                "基本認証を用いたHTTPリクエストの送信"
aliases:
- /ja/vba/sending-an-http-request-with-basic-authentication.md
date:                  2024-02-01T22:02:51.315074-07:00
model:                 gpt-4-0125-preview
simple_title:         "基本認証を用いたHTTPリクエストの送信"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/vba/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

Visual Basic for Applications（VBA）で基本認証を伴うHTTPリクエストを送信することは、ユーザー名とパスワードの資格情報で保護されたウェブリソースにアクセスすることに関わっています。プログラマーは、セキュアなAPIやウェブサービスとやり取りするためにこれを実行し、たとえばExcelやAccessでのタスクを自動化し、保護されたエンドポイントからデータを取得します。

## 方法：

VBAでは、`Microsoft XML, v6.0`（MSXML2）ライブラリを使用して、基本認証を伴うHTTPリクエストを送信できます。これには、資格情報をbase64でエンコードされた形式で含めるために、リクエストの`"Authorization"`ヘッダーを設定することが含まれます。以下はステップバイステップのガイドです：

1. **MSXML2を参照する**：まず、VBAプロジェクトが`Microsoft XML, v6.0`ライブラリを参照していることを確認します。VBAエディターで、ツール > 参照設定に移動し、`Microsoft XML, v6.0`をチェックします。

2. **HTTPリクエストを作成して送信する**：次のVBAコードスニペットをガイドとして使用してください。`"your_username"`と`"your_password"`を実際の資格情報に置き換え、必要に応じてURLを調整してください。

    ```vb
    Dim XMLHttp As Object
    Set XMLHttp = CreateObject("MSXML2.XMLHTTP")
    Dim url As String
    url = "http://example.com/api/resource" ' 実際のURLに置き換えてください
    Dim base64Credentials As String
    base64Credentials = EncodeBase64("your_username:your_password")
    
    XMLHttp.Open "GET", url, False
    XMLHttp.setRequestHeader "Authorization", "Basic " & base64Credentials
    XMLHttp.send
    
    Debug.Print XMLHttp.responseText ' 応答を即時ウィンドウに出力します
    ```

3. **資格情報をbase64でエンコードする**：VBAにはbase64エンコーディングのための組み込み関数はありませんが、このカスタム`EncodeBase64`関数を使用できます：

    ```vb
    Function EncodeBase64(text As String) As String
        Dim arrData() As Byte
        arrData = StrConv(text, vbFromUnicode)
        
        Dim objXML As MSXML2.DOMDocument60
        Dim objNode As MSXML2.IXMLDOMElement
        
        Set objXML = New MSXML2.DOMDocument60
        Set objNode = objXML.createElement("b64")
        
        objNode.dataType = "bin.base64"
        objNode.nodeTypedValue = arrData
        EncodeBase64 = objNode.Text
    End Function
    ```
    
これは、指定された基本認証資格情報で`http://example.com/api/resource`へGETリクエストを送信し、応答を出力します。

## 詳細解説

ここで使用されているアプローチは、シンプルなユースケースに効果的ですが、Basic Authentication方式に依存しており、資格情報は簡単にデコード可能な形式（base64エンコーディングは暗号化ではありません）で送信されます。HTTPSコンテキスト外での脆弱性のため、SSL/TLSのような追加のセキュリティレイヤーなしにインターネット経由で機密データを転送する場合、Basic Authenticationは推奨されません。

歴史的に、Basic Authenticationはウェブリソースへのアクセスを制御するために開発された最初の方法の一つでした。今日では、OAuth 2.0のように、新しいアプリケーションのためにはより安全かつ柔軟な認証基準が一般的に好まれます。VBAの制限とより高度な認証方法に必要な外部依存関係を考えると、開発者はしばしばVBAを内部またはセキュリティがそれほど重視されない環境で使用することがありますし、迅速にアイデアをプロトタイピングするための足がかりとして使用することもあります。

VBAでHTTPリクエストを使用する際には、MSXMLライブラリの各バージョンが異なる機能やセキュリティ基準をサポートしている可能性があることを覚えておいてください。常にアプリケーションと互換性のある最新バージョンを使用して、より良いセキュリティとパフォーマンスを確保してください。さらに、新しいプロジェクト、特に安全なHTTP通信が必要な場合にVBAを選択する際には、環境の制限や潜在的に廃止された機能も考慮してください。他のプログラミング環境や言語は、同様のタスクに対してより堅牢で、安全で、維持が容易な解決策を提供する場合があります。
