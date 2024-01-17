---
title:                "テキストファイルを作成する"
html_title:           "Java: テキストファイルを作成する"
simple_title:         "テキストファイルを作成する"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 何をするのか？何のために？
テキストファイルを書くことは、プログラマーがデータを保存したり共有したりするための方法です。プログラマーがテキストファイルを使用する最も一般的な目的は、データを永続化することです。つまり、プログラムが実行されていないときでもデータを保存することです。

## 方法：
```Java
import java.io.FileWriter; 
import java.io.IOException;

public class WriteTextFile {

    public static void main(String[] args) {
    
        // ファイルの作成と開く
        try {
            FileWriter myWriter = new FileWriter("myFile.txt");
            
            // データを書き込む
            myWriter.write("Hello World!");
            
            // ファイルを閉じる
            myWriter.close();
            
            // 完了メッセージを表示
            System.out.println("ファイルに書き込みました。");
            
        // エラー処理
        } catch (IOException e) {
            System.out.println("エラーが発生しました。");
            e.printStackTrace();
        }
    }
}
```
```
// ファイルに書き込みました。
```
このサンプルコードでは、最初に`FileWriter`クラスをインポートして、`myFile.txt`というテキストファイルを作成して開きます。`myWriter.write`を使用して、"Hello World!"という文字列をファイルに書き込みます。最後に、`myWriter.close`を使用してファイルを閉じます。エラーが発生した場合は、エラーメッセージが表示されます。

## 深堀り：
テキストファイルの書き込みは、プログラミングの初期から使用されている古い方法です。今では、データベースやクラウドストレージなどの他のオプションがあるため、テキストファイルを使用する必要はありません。しかし、小規模なプログラムやデータを永続化する場合は、テキストファイルが最も単純で便利な方法です。

## 参考：
- [Java FileWriter Class](https://www.w3schools.com/java/java_files_create.asp)
- [Java BufferedReader Class](https://www.w3schools.com/java/java_files_read.asp)
これらのリンクでは、`FileWriter`と`BufferedReader`クラスを使用してテキストファイルを作成し、読み取る方法が詳しく解説されています。また、オープンソースのコードレポジトリ「GitHub」でもテキストファイルを扱う多くのサンプルコードが閲覧できます。