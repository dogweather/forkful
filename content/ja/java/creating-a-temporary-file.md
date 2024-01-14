---
title:                "Java: 一時ファイルの作成"
simple_title:         "一時ファイルの作成"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## なぜ

一時ファイルを作成するのには、一時的にデータを保存したい場合や、一時的に変更を加えたい場合など、さまざまな理由があります。

## 作り方

一時ファイルを作成するには、Javaの標準ライブラリである`java.io.File`クラスを使用します。まず、一時ファイルを保存するためのディレクトリを作成し、そのディレクトリを指定して一時ファイルを作成します。

```Java
import java.io.File;
import java.io.IOException;

// 一時ファイルを保存するディレクトリを指定
String tempDir = "C:\\Temp";

try{
    // ディレクトリを作成
    File directory = new File(tempDir);
    directory.mkdir();
    
    // 一時ファイルを作成
    File tempFile = File.createTempFile("temp", ".txt", directory);
    
    // 一時ファイルが作成されたことを確認
    if(tempFile.exists()){
        System.out.println("一時ファイルが作成されました。");
        System.out.println("ファイル名：" + tempFile.getName());
        System.out.println("保存場所：" + tempFile.getAbsolutePath());
    }
}catch(IOException e){
    e.printStackTrace();
}
```

実行結果：

```
一時ファイルが作成されました。
ファイル名：temp5333298746363945609.txt
保存場所：C:\Temp\temp5333298746363945609.txt
```

## ディープダイブ

一時ファイルは、プログラムが終了すると自動的に削除されます。また、プログラムを実行するたびに、ファイル名がランダムに生成されるため、同じファイル名のファイルが作成される心配はありません。

さらに、一時ファイルはメモリよりもディスク容量を使用するため、大量のデータを扱う場合にも便利です。

## はじめてのJavaプログラミング [[日本語版]](https://www.amazon.co.jp/dp/B07SKJ58V5)

## See Also

- [Javaの標準ライブラリについて - 公式ドキュメント](https://docs.oracle.com/javase/jp/8/docs/api/)
- [一時ファイルの作成について - Java Tutorials 日本語版](https://docs.oracle.com/javase/tutorial/essential/io/file.html#create_temp_file)