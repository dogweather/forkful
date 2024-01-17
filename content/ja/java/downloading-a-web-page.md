---
title:                "「ウェブページのダウンロード」"
html_title:           "Java: 「ウェブページのダウンロード」"
simple_title:         "「ウェブページのダウンロード」"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 何が & 何故？

ウェブページをダウンロードするとは何かを説明します。プログラマーがそれを行う理由を説明します。
 
ウェブページをダウンロードするとは、インターネット上にあるページのコンテンツをローカルのコンピューターにコピーすることです。 プログラマーは、このようなタスクを自動化するためにウェブページをダウンロードします。 

## 方法：

次の ```Java ... ``` コードブロックで、コード例と出力例を示します。 

```Java 
// URL オブジェクトの作成
URL url = new URL("https://www.examplesite.com/");

// バッファリングされた文字入力ストリームの作成
BufferedReader br = new BufferedReader(new InputStreamReader(url.openStream()));

// 1 行ずつ文字列を読み込む
String line;
while ((line = br.readLine()) != null) {
   // 出力
   System.out.println(line);
}
// 接続を閉じる
br.close();
```

出力例:
```
<html>
<head>
<title>Example Website</title>
</head>
<body>
<h1>Welcome to Example Website!</h1>
<p>Thank you for visiting our website.</p>
</body>
</html>
```

## もっと詳しく：

ウェブページのダウンロードは、ウェブスクレイピングの一例です。 これは、ウェブサイトから情報を抽出するプロセスを指します。 ウェブスクレイピングには、JavaScript の実行や HTML のパースなどが必要になることがあります。

代替方法として、Java では HTTP クライアントライブラリを使用することもできます。 これにより、より高度な処理やログインの自動化などが可能になります。

ウェブページのダウンロードには、Java の URL クラスや HttpURLConnection クラスなどを使用することができます。 これらのクラスを使用すると、ウェブページをダウンロードする際に必要なヘッダーやクエリパラメータを設定できます。

## 関連リンク：

- [Java での URL ダウンロード方法](https://www.javatpoint.com/how-to-download-url-in-java)
- [ウェブスクレイピングについての Javatpoint のチュートリアル](https://www.javatpoint.com/web-scraping-in-java)
- [Apache HttpClient ライブラリのドキュメント](https://hc.apache.org/httpcomponents-client-ga/)