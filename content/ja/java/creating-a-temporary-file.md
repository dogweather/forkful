---
title:                "一時ファイルの作成"
html_title:           "Elixir: 一時ファイルの作成"
simple_title:         "一時ファイルの作成"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 何となぜ？

一時的なファイル作成は1度だけ使用するデータを格納するためのものです。プログラマーはこのファイルを使用して不要になったらすぐに消去し、システムリソースの無駄遣いを防ぐことができます。

## どうやって：

Javaでは、`java.nio.file`パッケージの`Files`クラスを使用して一時的なファイルを作成できます。以下に具体的なサンプルコードを示します。

```Java
import java.nio.file.Files;
import java.nio.file.Path;

public class Test {
    public static void main(String[] args) {
        try {
            Path tempFile = Files.createTempFile("myTempFile", ".txt");
            System.out.println("Temp file : " + tempFile.toString());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

上記のプログラムを実行すると、次のような結果が表示されます。

```Java
Temp file : /tmp/myTempFile123456789.txt
```

## 掘り下げる：

一時的なファイル作成はかつては手間とリソースを要する作業でしたが、Javaにはこれを効率的に行うツールが組み込まれています。これ以前には、各プログラマが自己責任で管理する必要がありました。

代替的には、`File`クラスの`createTempFile()`メソッドも使用できますが、効率性と拡張性の観点からは`Files`クラスが推奨されます。

Javaの一時ファイル作成は、名前と拡張子が自動で生成され、OSの一時ファイルディレクトリに保存されます。この実装により、ファイルはプログラム終了時に自動的に削除され、システムリソースが節約されます。

## 参照情報:

Javaのさらなる学習に役立つリンクを提供します：

- `Files.createTempFile`: [Oracle oficial documentation](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html#createTempFile-java.lang.String-java.lang.String-java.nio.file.attribute.FileAttribute...-)
- `java.nio.file`: [Oracle official documentation](https://docs.oracle.com/javase/7/docs/api/java/nio/file/package-summary.html)
- Temporary file in Java: [Java Code Geeks](https://www.javacodegeeks.com/2012/07/java-temporary-files.html)