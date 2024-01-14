---
title:                "Kotlin: 「ウェブページをダウンロードする」"
simple_title:         "「ウェブページをダウンロードする」"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

「なぜWebページのダウンロードに関わるのか？」

Webページのダウンロードは、インターネット上で情報やコンテンツを入手するために重要なプロセスです。例えば、オンラインショッピングやウェブサイトの閲覧など、さまざまな活動でダウンロードが必要になります。Kotlinの便利な機能を使って、簡単にWebページのダウンロードができることをご紹介します。

「方法」

まずは、必要なライブラリをインポートします。

```Kotlin
import java.net.URL
import java.io.File
```

次に、ダウンロードしたいWebページのURLを指定します。

```Kotlin
val url = URL("https://example.com")
```

ダウンロードしたページを保存するファイルを指定します。ここでは、'page.html'という名前で保存しています。

```Kotlin
val file = File("page.html")
```

最後に、`copyTo()`メソッドを使用してファイルをダウンロードします。

```Kotlin
url.openStream().use { input ->
    file.outputStream().use { output ->
        input.copyTo(output)
    }
}
```

これで、指定したURLのWebページが、`page.html`という名前のファイルとして保存されました。ダウンロードしたファイルの中身を確認すると、HTMLコードが表示されるはずです。

「詳細な説明」

これから詳しく説明するのは、KotlinでのWebページのダウンロードの仕組みです。

まずは、`URL`クラスを使用して、ダウンロードしたいWebページのURLを指定します。`java.net`パッケージに属するクラスで、URLに関する便利な機能を提供しています。

次に、`openStream()`メソッドを使用して、URLに接続します。これにより、`InputStream`オブジェクトが取得できます。このオブジェクトは、HTTPリクエストを送信し、レスポンスのデータを取得するためのものです。

そして、`copyTo()`メソッドを使用して、`InputStream`から取得したデータをファイルにコピーします。`outputStream()`メソッドを使用して、`File`オブジェクトの出力ストリームを取得し、そこにデータを書き込みます。`use`メソッドを使用して、ストリームを自動的に閉じることができます。

「関連情報」

- [Kotlinの公式ドキュメント](https://kotlinlang.org/docs/home.html)
- [java.net.URLクラスのドキュメント](https://docs.oracle.com/javase/8/docs/api/java/net/URL.html)
- [Kotlinでファイルを操作する方法](https://qiita.com/furu8ma/items/fe9a2a100dfc5d8299d5)