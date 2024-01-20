---
title:                "ウェブページのダウンロード"
html_title:           "Bash: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

タイトル: プログラミング：JavaによるWebページのダウンロード方法 

## 何となぜ？
Webページのダウンロードとは、通常、ウェブサイトの特定のページのHTMLをローカルストレージに保存することを指します。これはプログラマがウェブスクレイピング、ウェブコンテンツの分析、又はオフラインの参照のために行います。

## どうやるの？
以下のJavaコードは、URLからデータを取得し、そのコンテンツを出力します。

```Java
import java.net.URL;
import java.net.URLConnection;
import java.io.BufferedReader;
import java.io.InputStreamReader;

public class Main {
    public static void main(String[] args) throws Exception {
        URL url = new URL("http://www.example.com");
        URLConnection conn = url.openConnection();
        
        BufferedReader br = new BufferedReader(
                new InputStreamReader(conn.getInputStream()));
        
        String inputLine;
        
        while ((inputLine = br.readLine()) != null)
            System.out.println(inputLine);
        
        br.close();
    }
}
```

出力は試してみてください。provided URLのHTMLが表示されるはずです。

## 更に深く
**(1)歴史的な背景**:
Javaはもともと1995年にウェブアプリケーションの作成のために設計されました。それ以来、そのネットワーキング機能は非常に進化し、様々なウェブ関連のタスクに対応しています。

**(2)代替策**:
他のプログラミング言語、特にPythonやPHPはウェブスクレイピングとWebページのダウンロードに大変強力で多機能なライブラリを持っています。それにも関わらず、Javaの強力なオブジェクト指向機能と広いユーザベースが、多くの場合、その選択の理由となります。

**(3)実装の詳細**:
このコードはjava.netパッケージのクラスを使用しています。このパッケージはJavaの核となる部分であり、ネットワーキング機構の基本的なクラスとインターフェースを提供しています。

## 参照リンク
- Oracleの公式Javaドキュメント: [ここをクリック](https://docs.oracle.com/en/java/)
- Javaによるネットワーキングに関する詳細なチュートリアル: [ここをクリック](https://www.tutorialspoint.com/java/java_networking.htm)